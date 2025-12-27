package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.dto.TerminatedSellerSkuCleanupStatusDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;

public interface ProductDeletionService {

  /**
   * hard delete product data
   * @param product
   */
  void deleteProductData(Product product) throws Exception;

  /**
   * hard delete product attribute extracted
   * @param productCode
   */
  void hardDeleteProductAttributeExtracted(String productCode);

  /**
   * updated picked for deletion flag
   * @param product
   * @param pickedForDeletion
   */
  void updatePickedForDeletionFlag(Product product, boolean pickedForDeletion);

  /**
   * already picked for deletion
   * @param product
   * @return
   */
  boolean pickedForDeletion(Product product);

  /**
   * Is allowed to delete image or not
   * @param productItems
   * @return
   */
  boolean isAllowedToDeleteImage(List<ProductItem> productItems);

  /**
   * Publish event to update status and delete image
   * @param statusDTO
   */
  void publishEventToUpdateStatusAndDeleteImage(TerminatedSellerSkuCleanupStatusDTO statusDTO);
}
