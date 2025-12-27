package com.gdn.x.product.service.api;

import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;

public interface MarkForDeleteHelperService {

  ProductDetailResponse removeAllMarkForDelete(ProductDetailResponse product);

  ProductItemDetailResponse removeAllMarkForDelete(ProductItemDetailResponse productItem);

}
