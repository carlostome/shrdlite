
// URL to the Ajax CGI script:
var AjaxScript = "cgi-bin/ajaxwrapper.py";

// List of the JSON files that contain example worlds:
var ExampleNames = ["small","medium"];
var ExamplesFolder = "examples";

// What the system says when it has nothing to do:
var SystemPromptText = "What can I do for you today?";

// Constants that you can play around with:
var DialogueHistory = 100;    // max nr. utterances
var FloorThickness = 10;     // pixels
var WallSeparation = 4;     // pixels
var ArmSize = 0.2;         // of stack width
var AnimationPause = 0.1; // seconds
var PromptPause = 0.5;   // seconds
var AjaxTimeout = 5;    // seconds
var ArmSpeed = 1000;   // pixels per second

// This only has effect in the latest versions of Chrome and Safari,
// the only browsers that have implemented the W3C Web Speech API:
var UseSpeech = true;

// There is no way of setting male/female voice,
// so this is one way of having different voices for user/system:
var Voices = {"system": {"lang": "en-GB", "rate": 1.1}, // British English, slightly faster
              "user": {"lang": "en-US"},  // American English
             };


//==============================================================================
//
// Don't change anything below this line, if you don't know what you are doing.
//
//==============================================================================

var CanvasWidth;
var CanvasHeight;

var Pick = 'pick';
var Drop = 'drop';

var SvgNS = 'http://www.w3.org/2000/svg';

var ObjectData = {
    "brick":   {"small": {"width":0.30, "height":0.30},
                "large": {"width":0.70, "height":0.60},
               },
    "plank":   {"small": {"width":0.60, "height":0.10},
                "large": {"width":1.00, "height":0.15},
               },
    "ball":    {"small": {"width":0.30, "height":0.30},
                "large": {"width":0.70, "height":0.70},
               },
    "pyramid": {"small": {"width":0.60, "height":0.25},
                "large": {"width":1.00, "height":0.40},
               },
    "box":     {"small": {"width":0.60, "height":0.30, "thickness": 0.10},
                "large": {"width":1.00, "height":0.40, "thickness": 0.10},
               },
    "table":   {"small": {"width":0.60, "height":0.30, "thickness": 0.10},
                "large": {"width":1.00, "height":0.40, "thickness": 0.10},
               },
};

var ExampleWorlds;
var currentExample;
var currentWorld;
var currentPlan;
var currentArmPosition;

function stackWidth() {
    return CanvasWidth / currentWorld.world.length;
}

function boxSpacing() {
    return Math.min(5, stackWidth() / 20);
}

$(function() {
    $('#inputform').submit(function(){
        userInput();
        return false;
    });
    $('#inputexamples').change(function(){
        userInput();
        return false;
    });
    $('#showdebug').click(function(){
        $('#debug').toggle($('#showdebug').prop('checked'));
    });
    CanvasWidth = $("#svgdiv").width() - 2 * WallSeparation;
    CanvasHeight = $("#svgdiv").height() - FloorThickness;
    resetCurrentExample(ExampleNames[0]);
});

function loadExampleWorlds() {
    ExampleWorlds = {};
    $("#exampleworlds").empty();
    $.each(ExampleNames, function(i, name) {
        $('<input type="submit">').val(name)
            .click(changeCurrentExample)
            .appendTo($("#exampleworlds"));
        $.ajax({
            dataType: "json",
            url: ExamplesFolder + "/" + name + ".json",
            async: false
        }).fail(function(jqxhr, status, error) {
            alertError("Couldn't load example '" + name + "'.json: " + status, error);
        }).done(function(world) {
            ExampleWorlds[name] = world;
        });
    });
}

function changeCurrentExample() {
    var name = $(this).val();
    if (confirm('Are you certain that you want to reset to "' + name + '"?')) {
        resetCurrentExample(name);
    }
}

function resetCurrentExample(name) {
    loadExampleWorlds();
    currentExample = name;
    currentWorld = ExampleWorlds[currentExample];
    currentArmPosition = 0;
    $('#inputexamples').empty();
    $('#inputexamples').append($('<option value="">').text("(Select an example utterance)"));
    $.each(currentWorld.examples, function(i,value) {
        if (value instanceof Array) value = value.join(" ");
        $('#inputexamples').append($('<option>').text(value));
    });
    $("#dialogue > p").remove();
    resetSVG();
}

function resetSVG() {
    disableInput();
    $("#response").empty();
    sayUtterance("system", "Please wait while I populate the world.");
    $('#svgdiv').empty();

    var viewBox = [0, 0, CanvasWidth + 2 * WallSeparation, CanvasHeight + FloorThickness];
    var svg = $(SVG('svg')).attr({
        viewBox: viewBox.join(' '), 
        width: viewBox[2], 
        height: viewBox[3],
    }).appendTo($('#svgdiv'));

    // The floor:
    $(SVG('rect')).attr({
        x: 0,
        y: CanvasHeight,
        width: CanvasWidth + 2 * WallSeparation,
        height: CanvasHeight + FloorThickness,
        fill: 'black',
    }).appendTo(svg);

    // The arm:
    $(SVG('line')).attr({
        id:'arm',
        x1: stackWidth() / 2,
        y1: ArmSize * stackWidth() - CanvasHeight, 
        x2: stackWidth() / 2, 
        y2: ArmSize * stackWidth(), 
        stroke: 'black', 
        'stroke-width': ArmSize * stackWidth(),
    }).appendTo(svg);

    var timeout = 0;
    for (var stacknr=0; stacknr < currentWorld.world.length; stacknr++) {
        for (var objectnr=0; objectnr < currentWorld.world[stacknr].length; objectnr++) {
            var objectid = currentWorld.world[stacknr][objectnr];
            makeObject(svg, objectid, stacknr, timeout);
            timeout += AnimationPause;
        }
    }
    debugWorld();
    systemPrompt(timeout + PromptPause);
}

function SVG(tag) {
    return document.createElementNS(SvgNS, tag);
}

function animateMotion(object, path, timeout, duration) {
    if (path instanceof Array) {
        path = path.join(" ");
    }
    var animation = SVG('animateMotion');
    $(animation).attr({
        begin: 'indefinite',
        fill: 'freeze',
        path: path,
        dur: duration,
    }).appendTo(object);
    animation.beginElementAt(timeout);
    return animation;
}

function moveObject(action, stackNr) {
    if (action == Pick && currentWorld.holding) {
        alertError("ERROR", "I cannot pick an object from stack " + stackNr + ", I am already holding something!")
        return 0;
    } else if (action == Drop && !currentWorld.holding) {
        alertError("ERROR", "I cannot drop an object onto stack " + stackNr + ", I am not holding anything!")
        return 0;
    }
    var stack = currentWorld.world[stackNr];
    var arm = $('#arm');
    var xStack = stackNr * stackWidth() + WallSeparation;
    var xArm = currentArmPosition * stackWidth() + WallSeparation;

    if (action == Pick) {
        if (!stack.length) {
            alertError("ERROR", "I cannot pick an object from stack " + stackNr + ", it is empty!")
            return 0;
        }
        currentWorld.holding = stack.pop();
    }

    var altitude = getAltitude(stack);
    var objectHeight = getObjectDimensions(currentWorld.holding).heightadd;
    var yArm = CanvasHeight - altitude - ArmSize * stackWidth() - objectHeight;
    var yStack = -altitude;

    var path1 = ["M", xArm, 0, "H", xStack, "V", yArm];
    var path2 = ["M", xStack, yArm, "V", 0];
    var duration1 = (Math.abs(xStack - xArm) + Math.abs(yArm)) / ArmSpeed;
    var duration2 = (Math.abs(yArm)) / ArmSpeed;
    var anim1 = animateMotion(arm, path1, 0, duration1);
    var anim2 = animateMotion(arm, path2, duration1 + AnimationPause, duration2);

    if (action == Pick) {
        var path2b = ["M", xStack, yStack, "V", yStack-yArm];
        animateMotion($("#"+currentWorld.holding), path2b, duration1 + AnimationPause, duration2)
    } else if (action == Drop) {
        var path1b = ["M", xArm, yStack-yArm, "H", xStack, "V", yStack];
        animateMotion($("#"+currentWorld.holding), path1b, 0, duration1)
    }

    if (action == Drop) {
        stack.push(currentWorld.holding);
        currentWorld.holding = null;
    }
    currentArmPosition = stackNr;
    debugWorld();
    return duration1 + duration2 + 2 * AnimationPause;
}

function getObjectDimensions(objectid) {
    var attrs = currentWorld.objects[objectid];
    var size = ObjectData[attrs.form][attrs.size];
    var width = size.width * (stackWidth() - boxSpacing());
    var height = size.height * (stackWidth() - boxSpacing());
    var thickness = size.thickness * (stackWidth() - boxSpacing());
    var heightadd = attrs.form == 'box' ? thickness : height;
    return {
        width: width,
        height: height,
        heightadd: heightadd,
        thickness: thickness,
    };
}

function getAltitude(stack, objectid) {
    var altitude = 0;
    for (var i=0; i<stack.length; i++) {
        if (objectid == stack[i])
            break;
        altitude += getObjectDimensions(stack[i]).heightadd + boxSpacing();
    }
    return altitude;
}

function makeObject(svg, objectid, stacknr, timeout) {
    var attrs = currentWorld.objects[objectid];
    var altitude = getAltitude(currentWorld.world[stacknr], objectid);
    var dim = getObjectDimensions(objectid);

    var ybottom = CanvasHeight - boxSpacing();
    var ytop = ybottom - dim.height;
    var ycenter = (ybottom + ytop) / 2;
    var yradius = (ybottom - ytop) / 2;
    var xleft = (stackWidth() - dim.width) / 2
    var xright = xleft + dim.width;
    var xcenter = (xright + xleft) / 2;
    var xradius = (xright - xleft) / 2;
    var xmidleft = (xcenter + xleft) / 2;
    var xmidright = (xcenter + xright) / 2;

    var object;
    switch (attrs.form) {
    case 'brick':
    case 'plank':
        object = $(SVG('rect')).attr({
            x: xleft, 
            y: ytop, 
            width: dim.width, 
            height: dim.height
        });
        break;
    case 'ball':
        object = $(SVG('ellipse')).attr({
            cx: xcenter, 
            cy: ycenter, 
            rx: xradius, 
            ry: yradius
        });
        break;
    case 'pyramid':
        var points = [xleft, ybottom, xmidleft, ytop, xmidright, ytop, xright, ybottom];
        object = $(SVG('polygon')).attr({
            points: points.join(" ")
        });
        break;
    case 'box':
        var points = [xleft, ytop, xleft, ybottom, xright, ybottom, xright, ytop, 
                      xright-dim.thickness, ytop, xright-dim.thickness, ybottom-dim.thickness,
                      xleft+dim.thickness, ybottom-dim.thickness, xleft+dim.thickness, ytop];
        object = $(SVG('polygon')).attr({
            points: points.join(" ")
        });
        break;
    case 'table':
        var points = [xleft, ytop, xright, ytop, xright, ytop+dim.thickness, 
                      xmidright, ytop+dim.thickness, xmidright, ybottom, 
                      xmidright-dim.thickness, ybottom, xmidright-dim.thickness, ytop+dim.thickness,
                      xmidleft+dim.thickness, ytop+dim.thickness, xmidleft+dim.thickness, ybottom,
                      xmidleft, ybottom, xmidleft, ytop+dim.thickness, xleft, ytop+dim.thickness];
        object = $(SVG('polygon')).attr({
            points: points.join(" ")
        });
        break;
    }
    object.attr({
        id: objectid,
        stroke: 'black', 
        'stroke-width': boxSpacing() / 2, 
        fill: attrs.color, 
    });
    object.appendTo(svg);

    var path = ["M", stacknr * stackWidth() + WallSeparation, -(CanvasHeight + FloorThickness)];
    animateMotion(object, path, 0, 0);
    path.push("V", -altitude);
    animateMotion(object, path, timeout, 0.5);
}

function disableInput(timeout) {
    if (timeout) {
        setTimeout(disableInput, 1000*timeout);
    } else {
        $("#inputexamples").blur();
        $("#inputexamples").prop('disabled', true); 
        $("#userinput").blur();
        $("#userinput").prop('disabled', true); 
    }
}

function systemPrompt(timeout) {
    if (timeout) {
        setTimeout(systemPrompt, 1000*timeout);
    } else {
        sayUtterance("system", SystemPromptText);
        enableInput();
    }
}

function enableInput() {
    $("#inputexamples").prop('disabled', false).val(''); 
    $("#inputexamples option:first").attr('selected','selected');
    $("#userinput").prop('disabled', false); 
    $("#userinput").focus().select();
}

function performPlan() {
    if (currentPlan && currentPlan.length) {
        var item = currentPlan.shift();
        var timeout = 0;
        var action = getAction(item);
        if (action) {
            timeout = moveObject(action[0], action[1]);
        } else if (item && item[0] != "#") {
            if (window.speechSynthesis.speaking) {
                currentPlan.unshift(item);
                timeout = AnimationPause;
            } else {
                sayUtterance("system", item);
            }
        }
        setTimeout(performPlan, 1000 * timeout);
    } else {
        systemPrompt(PromptPause);
    }
}

function getAction(item) {
    if (typeof(item) == "string") item = item.trim().split(/\s+/);
    if (item.length == 2 &&
        (item[0] == Pick || item[0] == Drop) &&
        /^\d+$/.test(item[1]))
    {
        item[1] = parseInt(item[1]);
        return item;
    }
    return null;
}

function splitAction(action) {
}

function userInput() {
    var userinput = $("#inputexamples").val();
    if (userinput) {
        $("#userinput").val(userinput.trim());
        enableInput();
        return;
    }
    userinput = $("#userinput").val().trim();
    if (!userinput) {
        enableInput();
        return;
    }
    disableInput();

    sayUtterance("user", userinput);

    var ajaxdata = {'world': currentWorld.world,
                    'objects': currentWorld.objects,
                    'holding': currentWorld.holding,
                    'state': currentWorld.state,
                    'utterance': userinput.split(/\s+/)
                   };

    $.ajax({
        url: AjaxScript,
        dataType: "text",
        cache: false,
        timeout: 1000 * AjaxTimeout,
        data: {'data': JSON.stringify(ajaxdata)}
    }).fail(function(jqxhr, status, error) {
        alertError("Internal error: " + status, error);
        systemPrompt();
    }).done(function(result) {
        try {
            result = JSON.parse(result);
        } catch(err) {
            alertError("JSON error:" + err, result);
        }
        debugResult(result);
        sayUtterance("system", result.output);
        if (result.state) {
            currentWorld.state = result.state;
        }
        currentPlan = result.plan;
        performPlan();
    });
}

function sayUtterance(participant, utterance, silent) {
    var dialogue = $("#dialogue");
    if (dialogue.children().length > DialogueHistory) {
        dialogue.children().first().remove();
    }
    $('<p>').attr("class", participant)
        .text(utterance)
        .insertBefore($("#inputform"));
    dialogue.scrollTop(dialogue.prop("scrollHeight"));
    if (UseSpeech && !silent) {
        try {
            // W3C Speech API (works in Chrome and Safari)
            var speech = new SpeechSynthesisUtterance(utterance);
            for (var attr in Voices[participant]) {
                speech[attr] = Voices[participant][attr];
            }
            console.log("SPEAKING: " + utterance);
            window.speechSynthesis.speak(speech);
        } catch(err) {
        }
    }
}

function debugWorld() {
    $("#debugworld").html("<table><tr><td>&nbsp;" + currentWorld.world.join("&nbsp;<td>&nbsp;") + "&nbsp;</tr></table>");
    $("#debugholding").html(currentWorld.holding || "&mdash;");
}

function debugResult(result) {
    $("#debugoutput").text(result.output);
    $("#debugtrees").html(result.trees ? result.trees.join("<br>") : "&mdash;");
    $("#debuggoals").html(result.goals ? result.goals.join("<br>") : "&mdash;");
    $("#debugplan").html(result.plan ? result.plan.join("<br>") : "&mdash;");
    $("#debugstate").html(result.state ? JSON.stringify(result.state) : "&mdash;");
    $("#debugjson").text(JSON.stringify(result, null, " "));
}

function alertError(title, description) {
    if (typeof(description) !== "string") description = JSON.stringify(description);
    sayUtterance("error", "[" + title + "] " + description, true);
    console.log("*** " + title + " ***");
    console.log(description);
}
