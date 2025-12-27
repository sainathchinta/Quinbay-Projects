package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.dto.ActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductService;

public class ImageServiceWrapperImplTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String LOCATION_PATH = "LOCATION_PATH";
  private static final String HASH_CODE = "HASH_CODE";
  private static final String ID = "ID";

  @InjectMocks
  private ImageServiceWrapperImpl imageServiceWrapper;

  @Mock
  private ImageService imageService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private ProductService productService;

  @Mock
  private DomainEventPublisherService publisherService;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(imageService);
    verifyNoMoreInteractions(applicationCacheServiceBean);
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(publisherService);
  }

  private ProductActivateImageDTO generateProductActivationDTO() {
    ProductActivateImageDTO productActivateImageDTO = new ProductActivateImageDTO();
    productActivateImageDTO.setProductCode(PRODUCT_CODE);
    Set<ActivateImageDTO> activateImageDTOS = new HashSet<>();
    ActivateImageDTO dto = new ActivateImageDTO();
    dto.setFilenames(LOCATION_PATH);
    dto.setHashCode(HASH_CODE);
    dto.setProductCode(PRODUCT_CODE);
    dto.setCommonImage(true);
    activateImageDTOS.add(dto);
    productActivateImageDTO.setImageRequests(activateImageDTOS);
    return productActivateImageDTO;
  }

  @Test
  public void activateAndUpdateImagesNameTest() throws Exception {
    ProductActivateImageDTO productActivateImageDTO = generateProductActivationDTO();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO();
    Product product = new Product();
    product.setId(ID);
    productPublishUpdateDTO.setProduct(product);
    Mockito.when(this.imageService.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, true))
        .thenReturn(productPublishUpdateDTO);
    Mockito.when(this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    imageServiceWrapper.activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, true);
    Mockito.verify(this.imageService).activateAndUpdateImagesName(STORE_ID, productActivateImageDTO, true);
    Mockito.verify(applicationCacheServiceBean).evictProductImagesCacheByStoreIdAndProductId(STORE_ID, ID);
    Mockito.verify(applicationCacheServiceBean).evictProductItemImagesCacheByStoreIdAndProductId(STORE_ID, ID);
    Mockito.verify(this.productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.publisherService)
        .publishProductChangeCategory(productPublishUpdateDTO, null, false, false, false, false, false, new HashSet<>(), false,
            false, new HashSet<>());
  }

}
