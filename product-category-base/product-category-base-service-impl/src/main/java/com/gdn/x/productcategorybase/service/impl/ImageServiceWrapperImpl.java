package com.gdn.x.productcategorybase.service.impl;

import java.util.HashSet;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ImageServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;

@Service
public class ImageServiceWrapperImpl implements ImageServiceWrapper {

  @Autowired
  private ImageService imageService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Override
  public void activateAndUpdateImagesName(String storeId, ProductActivateImageDTO dto, boolean skipReview)
      throws Exception {
    ProductPublishUpdateDTO productPublishUpdateDTO =
        this.imageService.activateAndUpdateImagesName(storeId, dto, skipReview);
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId,
        productPublishUpdateDTO.getProduct().getId());
    applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(storeId,
        productPublishUpdateDTO.getProduct().getId());
    Product product =
        this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, dto.getProductCode());
    productPublishUpdateDTO.setProduct(product);
    domainEventPublisherService.publishProductChangeCategory(productPublishUpdateDTO, null, false, false, false, false,
        false, new HashSet<>(), false, false, new HashSet<>());
  }

}
