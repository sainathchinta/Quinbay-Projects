package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;

public interface ImageServiceWrapper {

  void activateAndUpdateImagesName(String storeId, ProductActivateImageDTO dto, boolean skipReview) throws Exception;

}
