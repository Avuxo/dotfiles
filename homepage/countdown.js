/*list of shows that will be put in the homepage*/
var shows = [
    {'show' : "Steins-Gate-0",  'targetDay' : 3, 'targetTime' : 10},
];

function timeToAir(show){

    var now = new Date();
    // the time targeted
    var targetTime = new Date(now.getFullYear(), now.getMonth(),
                              now.getDate()-now.getDay() + show.targetDay, show.targetTime);

    var deltaTime = targetTime.getTime() - now.getTime();
    if(deltaTime <= 0){
        targetTime = new Date(now.getFullYear(), now.getMonth(),
                              now.getDate()-now.getDay() + (show.targetDay + 7), show.targetTime);
        deltaTime = targetTime.getTime() - now.getTime();
        console.log(targetTime);
    }

    // calculate the different times.
    var seconds = Math.floor(deltaTime / 1000);
    var minutes = Math.floor(seconds / 60);
    var hours   = Math.floor(minutes / 60);
    var days    = Math.floor(hours / 24);

    // correct negative values and set them to be positive values
    
    seconds %= 60;
    minutes %= 60;
    hours   %= 24;

    return days + ":" + hours + ":" + minutes + ":" + seconds;
}

function updateCountdown(show){
    var time = timeToAir(show);
    var name = show.show;

    document.getElementById(name).innerHTML = time;
}

function updateText(){
    shows.forEach(function(i){
        updateCountdown(i);
    });
}

 setInterval(updateText, 1000);

document.onkeydown = function(e){
    switch(event.keyCode){
    case 77: window.location.href = "https://gmail.com/";                       break;
    case 89: window.location.href = "https://youtube.com";                      break;
    case 72: window.location.href = "https://news.ycombinator.com";             break;
    case 80: window.location.href = "https://reddit.com/r/programming/";        break;
    case 76: window.location.href = "https://lainchan.org/%CE%BB/catalog.html"; break;
    case 71: window.location.href = "https://github.com/avuxo/";                break;
    case 52: window.location.href = "https://webmail.gandi.net/";               break;
        
    }
}


