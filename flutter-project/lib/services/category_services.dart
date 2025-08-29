import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/product/category_all_response.dart';
import 'package:ktc_logistics_driver/domain/models/common/response_default.dart';

class CategoryServices {

  final _env = Environment.getInstance();

  Future<ResponseDefault> addNewCategory(String nameCategory, String descriptionCategory) async {

    final token = await secureStorage.readToken();

    final response = await http.post(Uri.parse('${_env.endpointApi}/add-categories'),
      headers: {  'Accept' : 'application/json', 'xx-token' : token! },
      body: {
        'category' : nameCategory,
        'description' : descriptionCategory
      }
    );

    return ResponseDefault.fromJson( jsonDecode( response.body ) );
  } 


  Future<List<Category>> getAllCategories() async {

    final token = await secureStorage.readToken();

    final response = await http.get(Uri.parse('${_env.endpointApi}/get-all-categories'),
      headers: {  'Accept' : 'application/json', 'xx-token' : token! }
    );

    return CategoryAllResponse.fromJson( jsonDecode( response.body )).categories;
  }


  Future<ResponseDefault> deleteCategory(String uidCategory) async {

    final token = await secureStorage.readToken();

    final resp = await http.delete(Uri.parse('${_env.endpointApi}/delete-category/$uidCategory' ),
      headers: { 'Content-type' : 'application/json', 'xx-token' : token! }
    );

    return ResponseDefault.fromJson( jsonDecode( resp.body ) );
  }

  

}

final categoryServices = CategoryServices();


