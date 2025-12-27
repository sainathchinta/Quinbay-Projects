package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.repository.ProductItemUomInfoRepository;
import com.gdn.x.productcategorybase.service.ProductItemUomInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ProductItemUomInfoServiceBean implements ProductItemUomInfoService {

  @Autowired
  private ProductItemUomInfoRepository productItemUomInfoRepository;

  @Override
  public List<ProductItemUomInfo> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode) {
    return productItemUomInfoRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
  }

  @Override
  public List<ProductItemUomInfo> findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(String storeId,
      List<String> skuCodes) {
    return productItemUomInfoRepository.findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(storeId, skuCodes);
  }
}
