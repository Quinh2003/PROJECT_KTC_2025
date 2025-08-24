import 'package:flutter/material.dart';
import 'package:flutter_bloc/flutter_bloc.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/presentation/blocs/blocs.dart';
import 'package:ktc_logistics_driver/domain/models/response/products_top_home_response.dart';
import 'package:ktc_logistics_driver/services/products_services.dart';
import 'package:ktc_logistics_driver/presentation/components/components.dart';
import 'package:ktc_logistics_driver/presentation/helpers/helpers.dart';
import 'package:ktc_logistics_driver/presentation/screens/admin/products/add_new_product_screen.dart';
import 'package:ktc_logistics_driver/presentation/themes/colors_frave.dart';

class ListProductsScreen extends StatefulWidget {

  @override
  State<ListProductsScreen> createState() => _ListProductsScreenState();
}

class _ListProductsScreenState extends State<ListProductsScreen> {

  @override
  Widget build(BuildContext context) {
    return BlocListener<ProductsBloc, ProductsState>(
      listener: (context, state) {
        if( state is LoadingProductsState ){
          modalLoading(context);
        }
        if( state is SuccessProductsState ){
          Navigator.pop(context);
          modalSuccess(context, 'Success', (){
            Navigator.pop(context);
            setState(() {});
          });
        }
        if(state is FailureProductsState ){
          Navigator.pop(context);
          errorMessageSnack(context, state.error);
        }
      },
      child: Scaffold(
        backgroundColor: Colors.white,
        appBar: AppBar(
          backgroundColor: Colors.white,
          title: const TextCustom(text: 'List Products', fontSize: 19),
          centerTitle: true,
          leadingWidth: 80,
          elevation: 0,
          leading: InkWell(
            onTap: () => Navigator.pop(context),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: const [
                Icon(Icons.arrow_back_ios_new_rounded, color: ColorsFrave.primaryColor, size: 17),
                TextCustom(text: 'Back', fontSize: 17, color: ColorsFrave.primaryColor)
              ],
            ),
          ),
          actions: [
            TextButton(
              onPressed: () => Navigator.push(context, routeFrave(page: AddNewProductScreen())), 
              child: const TextCustom(text: 'Add', fontSize: 17, color: ColorsFrave.primaryColor)
            )
          ],
        ),
        body: FutureBuilder<List<Productsdb>>(
          future: productServices.listProductsAdmin(),
          builder: (context, snapshot) 
            => ( !snapshot.hasData )
              ? const ShimmerFrave()
              : _GridViewListProduct(listProducts: snapshot.data!)
           
        ),
      ),
    );
  }
}

class _GridViewListProduct extends StatelessWidget {
  
  final List<Productsdb> listProducts;

  const _GridViewListProduct({required this.listProducts});

  @override
  Widget build(BuildContext context) {
    final _env = Environment.getInstance();

    return GridView.builder(
      physics: const BouncingScrollPhysics(),
      padding: const EdgeInsets.symmetric(horizontal: 20.0, vertical: 10.0),
      itemCount: listProducts.length,
      gridDelegate: SliverGridDelegateWithMaxCrossAxisExtent(
        maxCrossAxisExtent: 200,
        childAspectRatio: 4,
        crossAxisSpacing: 20,
        mainAxisSpacing: 20
      ), 
      itemBuilder: (context, i) 
        => InkWell(
          onTap: () => modalActiveOrInactiveProduct(context, listProducts[i].status, listProducts[i].nameProduct, listProducts[i].id, listProducts[i].picture),
          onLongPress: () => modalDeleteProduct(context, listProducts[i].nameProduct, listProducts[i].picture, listProducts[i].id.toString()),
          child: Row(
            children: [
              Container(
                height: 50,
                width: 50,
                decoration: BoxDecoration(
                  image: DecorationImage(
                    scale: 7,
                    image: NetworkImage('${_env.endpointBase}${listProducts[i].picture}')
                  )
                ),
              ),
              Expanded(
                child: Container(
                  alignment: Alignment.center,
                  decoration: BoxDecoration(
                    borderRadius: BorderRadius.circular(5.0),
                    color: ( listProducts[i].status == 1 ) ? Colors.grey[50] : Colors.red[100]
                  ),
                  child: TextCustom(text: listProducts[i].nameProduct, fontSize: 16),
                ),
              ),
            ],
          ),
        ),
    );  

  }



}
