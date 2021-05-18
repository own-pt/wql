// Sentences container in HTML
var divSents = document.getElementById('sents-cont');
// Object with the sentences:
var sentObj = $.getJSON('./list', (data, sucess) => {
    $.each(data, (key, val) => {
        let sentText = document.createElement('p');
        sentText.innerHTML = `<strong>${key}.</strong> ${val}`;
        divSents.appendChild(sentText);
    })
});
// var x = Object.values(sentObj.responseJSON);
// x.forEach(element => {
//     sentText = document.createElement('p');
//     [id, text] = element;
//     sentText.innerHTML = `<strong>${id}.</strong> ${text}`;
//     divSents.appendChild(sentText);
// });
