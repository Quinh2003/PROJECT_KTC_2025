import 'dart:async';
import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:image_picker/image_picker.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/response/images_products_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/products_top_home_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/response_default.dart';
import 'package:ktc_logistics_driver/presentation/helpers/de_bouncer.dart';


class ProductsServices {

  final debouncer = DeBouncer(duration: Duration(milliseconds: 800));
  final StreamController<List<Productsdb>> _streamController = StreamController<List<Productsdb>>.broadcast(); 
  Stream<List<Productsdb>> get searchProducts => _streamController.stream;
  
  // Environment instance
  final _env = Environment.getInstance();

  void dispose() {
    _streamController.close();
  }

  Future<ResponseDefault> addNewProduct(String name, String description, String price, List<XFile> images, String category ) async {

    final token = await secureStorage.readToken();

    var request = http.MultipartRequest('POST', Uri.parse('${_env.endpointApi}/add-new-products'))
      ..headers['Accept'] = 'application/json'
      ..headers['xx-token'] = token!
      ..fields['name'] = name
      ..fields['description'] = description
      ..fields['price'] = price
      ..fields['category'] = category;
      for (var image in images) {
        request.files.add( await http.MultipartFile.fromPath('image', image.path));
      }

    final response = await request.send();
    var data = await http.Response.fromStream(response);

    return ResponseDefault.fromJson(jsonDecode(data.body));
  }


  Future<List<Productsdb>> getProductsTopHome() async {

    final token = await secureStorage.readToken();

    final response = await http.get(Uri.parse('${_env.endpointApi}/get-products-top-home'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ProductsTopHomeResponse.fromJson(jsonDecode(response.body)).productsdb;
  }


  Future<List<ImageProductdb>> getImagesProducts(String id) async {

    final token = await secureStorage.readToken();

    final response = await http.get(Uri.parse('${_env.endpointApi}/get-images-products/$id'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ImagesProductsResponse.fromJson(jsonDecode(response.body)).imageProductdb;
  }  


  void searchProductsForName(String productName) async {

    debouncer.value = '';
    debouncer.onValue = ( value ) async {

      final token = await secureStorage.readToken();

      final response = await http.get(Uri.parse('${_env.endpointApi}/search-product-for-name/$productName'),
        headers: { 'Accept' :  'application/json', 'xx-token' : token! }
      );

      final listProduct =  ProductsTopHomeResponse.fromJson( jsonDecode( response.body) ).productsdb;

      this._streamController.add(listProduct);

    };
    final timer = Timer(Duration(milliseconds: 200), () => debouncer.value = productName);
    Future.delayed(Duration(milliseconds: 400)).then((_) => timer.cancel());

  }


  Future<List<Productsdb>> searchPorductsForCategory(String idCategory) async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/search-product-for-category/$idCategory'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ProductsTopHomeResponse.fromJson(jsonDecode(resp.body)).productsdb;
  }


  Future<List<Productsdb>> listProductsAdmin() async {

    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.endpointApi}/list-porducts-admin'),
      headers: {'Content-Type' : 'application/json', 'xx-token' : token!}
    );

    return ProductsTopHomeResponse.fromJson(jsonDecode(resp.body)).productsdb;
  }


  Future<ResponseDefault> updateStatusProduct(String idProduct, String status) async {

    final token = await secureStorage.readToken();

    final resp = await http.put(Uri.parse('${_env.endpointApi}/update-delivered-product'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
      body: {
        'idProduct' : idProduct,
        'status' : status
      }
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }


  Future<ResponseDefault> deleteProduct(String idProduct) async {

    final token = await secureStorage.readToken();

    final resp = await http.delete(Uri.parse('${_env.endpointApi}/delete-product/$idProduct'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }




}

final productServices = ProductsServices();


