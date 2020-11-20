$(function() {
  // For card rotation
  $('#btn-flip-front-id').click(function(){
    $('.card-front-id').addClass(' rotate-card-front-id');
    $('.card-back-id').addClass(' rotate-card-back-id');
    });

    $('#btn-flip-back-id').click(function(){
      $('.card-front-id').removeClass(' rotate-card-front-id');
      $('.card-back-id').removeClass(' rotate-card-back-id');
    });
});

$(function() {

  $('#go_to_back').click(function(){
  $('#go_to_back').hide();
  $('#go_to_front').show();
  });

  $('#go_to_front').click(function(){
  $('#go_to_front').hide();
  $('#go_to_back').show();
  });
});




