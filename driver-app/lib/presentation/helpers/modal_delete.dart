import 'package:flutter/material.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';

void modalDelete(BuildContext context, String name, String image, VoidCallback onPressed){
  final _env = Environment.getInstance();

  showDialog(
    context: context,
    barrierColor: Colors.white54, 
    builder: (context) 
      => AlertDialog(
        shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(15.0)),
        content: SizedBox(
          height: 196,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Row(
                    children: const [
                      TextCustom(text: 'Frave ', color: Colors.red, fontWeight: FontWeight.w500 ),
                      TextCustom(text: 'Food', fontWeight: FontWeight.w500),
                    ],
                  ),
                  InkWell(
                    onTap: () => Navigator.pop(context),
                    child: const Icon(Icons.close)
                  )
                ],
              ),
              const Divider(),
              const SizedBox(height: 10.0),
              const TextCustom(text: 'Are you sure?'),
              const SizedBox(height: 20.0),
              Row(
                children: [
                  Container(
                    height: 40,
                    width: 40,
                    decoration: BoxDecoration(
                      image: DecorationImage(
                        scale: 7,
                        image: NetworkImage('${_env.endpointBase}$image')
                      )
                    ),
                  ),
                  const SizedBox(width: 10.0),
                  TextCustom(
                    text: name,
                    maxLine: 2,
                  ),
                ],
              ),
              const SizedBox(height: 20.0),
              BtnFrave(
                height: 45,
                color: Colors.red,
                text: 'DELETE',
                fontWeight: FontWeight.bold,
                onPressed: onPressed,
              )
            ],
          ),
        ),
      ),
  );

}


