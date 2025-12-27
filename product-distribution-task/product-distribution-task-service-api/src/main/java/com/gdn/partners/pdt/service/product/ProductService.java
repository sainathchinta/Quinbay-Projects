package com.gdn.partners.pdt.service.product;

import com.gdn.x.mta.distributiontask.model.Product;

public interface ProductService {

  void overwrite(Product savedProduct, Product product) throws Exception;

}
