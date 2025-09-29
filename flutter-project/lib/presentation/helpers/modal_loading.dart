import 'package:flutter/material.dart';

void modalLoading(BuildContext context){

  showDialog(
    context: context,
    barrierDismissible: false,
    barrierColor: Colors.white54, 
    builder: (context) 
      => AlertDialog(
        backgroundColor: Colors.transparent,
        elevation: 0,
        content: Center(
          child: Container(
            padding: const EdgeInsets.all(20),
            decoration: BoxDecoration(
              color: Colors.white,
              borderRadius: BorderRadius.circular(15.0),
              boxShadow: [
                BoxShadow(
                  color: Colors.black.withOpacity(0.1),
                  blurRadius: 10,
                  spreadRadius: 1,
                )
              ]
            ),
            child: const CircularProgressIndicator(),
          ),
        ),
      ),
  );

}


