package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import org.springframework.data.domain.Page;

import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;

public interface ProductItemServiceWrapper {

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductItemsWithProductItemImagesCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(String storeId, Product product,
      boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setCompleteProductItemDetailsCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductItemsWithProductItemAttributeValuesAndAttributeCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductItemsCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param upcCodeSearchRequest
   * @param isOnlyExternal
   * @param page
   * @param size
   * @return
   */
  Page<ProductItem> findByUPCCodeAndCategoryIds(
      String storeId, UPCCodeSearchRequest upcCodeSearchRequest, boolean isOnlyExternal, Integer page, Integer size);

  /**
   *
   * @param storeId storeID
   * @param product product
   * @param includeMarkForDelete includeMarkForDelete
   * @return
   */
  void setProductItemImagesWithoutFilteringMainImages(String storeId, Product product,
    boolean includeMarkForDelete);

  /**
   * find product item images by item codes
   *
   * @param storeId
   * @param itemCode
   * @param fetchImageResponse boolean
   * @return
   */
  List<ItemImageResponse> findProductItemImagesByItemCodes(String storeId, List<String> itemCode, boolean originalImages,
    Boolean fetchImageResponse);

  /**
   *
   * Update UPC code in Product Item
   * @param storeId storeId
   * @param request list of ProductItemUpcCodeUpdateRequest
   * @param productCode product Code
   * @throws
   */
  void updateProductItemUpcCodeAndEvictCache(String storeId, List<ProductItemUpcCodeUpdateRequest> request, String productCode)
    throws Exception;
}
