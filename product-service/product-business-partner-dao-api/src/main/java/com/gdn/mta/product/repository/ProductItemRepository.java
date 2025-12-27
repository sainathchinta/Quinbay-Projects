package com.gdn.mta.product.repository;

import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.entity.ProductItem;

import java.util.List;

public interface ProductItemRepository {

  Page<ProductItem> findByStoreIdAndKeywordAndViewable(String storeId, String keyword, boolean viewable,
      boolean isOnlyExternal, Pageable pageable) throws Exception;

  Page<ProductItem> findByStoreIdAndUpcCode(String storeId, String upcCode, Pageable pageable) throws Exception;
  
  Page<ProductCodeResponse> findByStoreIdAndNameOrUpcCode(String storeId, String productName,
      String upcCode, String finalCategoryId, List<AttributeReqModel> modelList, Pageable pageable)
          throws Exception;

  Page<ProductItem> findByStoreIdAndUpcCodeExactMatch(String storeId, String upcCode,
      Pageable pageable) throws Exception;
  
  Page<ProductItem> findByStoreIdAndProductItemNameAndCategoryId(String storeId, String itemName,
      String categoryId, Pageable pageable) throws Exception;
}
