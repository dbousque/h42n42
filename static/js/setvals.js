

function setVals() {
	$("input").each(function(ind, inp) {
		inp.setAttribute("value", inp.value)
	})
}