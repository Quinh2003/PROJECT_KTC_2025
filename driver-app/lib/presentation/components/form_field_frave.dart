import 'package:flutter/material.dart';
import 'package:google_fonts/google_fonts.dart';

class FormFieldFrave extends StatelessWidget {
  
  final TextEditingController? controller;
  final String? hintText;
  final bool isPassword;
  final TextInputType keyboardType;
  final int maxLine;
  final bool readOnly;
  final FormFieldValidator<String>? validator;
  final Widget? prefixIcon;
  final BorderRadius? borderRadius;
  final EdgeInsetsGeometry? contentPadding;
  final Color? fillColor;

  const FormFieldFrave({super.key,  
    this.controller, 
    this.hintText, 
    this.isPassword = false,
    this.keyboardType = TextInputType.text,
    this.maxLine = 1,
    this.readOnly = false,
    this.validator,
    this.prefixIcon,
    this.borderRadius,
    this.contentPadding,
    this.fillColor,
  });

  @override
  Widget build(BuildContext context) {
    return TextFormField(
      controller: controller,
      style: GoogleFonts.getFont('Roboto', fontSize: 16),
      obscureText: isPassword,
      maxLines: maxLine,
      readOnly: readOnly,
      keyboardType: keyboardType,
      decoration: InputDecoration(
        border: OutlineInputBorder(
          borderRadius: borderRadius ?? BorderRadius.circular(5.0),
          borderSide: BorderSide(width: 0.5, color: Colors.grey.withOpacity(0.5)),
        ),
        enabledBorder: OutlineInputBorder(
          borderRadius: borderRadius ?? BorderRadius.circular(5.0),
          borderSide: BorderSide(width: 0.5, color: Colors.grey.withOpacity(0.5)),
        ),
        focusedBorder: OutlineInputBorder(
          borderRadius: borderRadius ?? BorderRadius.circular(5.0),
          borderSide: const BorderSide(width: 1.0, color: Color(0xFF5117AC)),
        ),
        errorBorder: OutlineInputBorder(
          borderRadius: borderRadius ?? BorderRadius.circular(5.0),
          borderSide: BorderSide(width: 1.0, color: Colors.red.shade300),
        ),
        contentPadding: contentPadding ?? const EdgeInsets.only(left: 15.0),
        hintText: hintText,
        hintStyle: GoogleFonts.getFont('Roboto', color: Colors.grey),
        prefixIcon: prefixIcon,
        filled: fillColor != null,
        fillColor: fillColor,
      ),
      validator: validator,
    );
  }
}


