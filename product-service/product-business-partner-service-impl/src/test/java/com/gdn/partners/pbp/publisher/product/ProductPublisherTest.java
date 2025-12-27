package com.gdn.partners.pbp.publisher.product;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.util.HashMap;
import java.util.Map;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.ProductPublisherService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class ProductPublisherTest {

  private static final String DEFAULT_PRODUCT_CODE_1 = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_CODE_2 = "MTA-0000002";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String RESTRICTED_KEYWORDS_FIELD =
      "[{\"fieldIdentifier\" : \"PRODUCT_NAME\", \"keywords\" : [\"abc@gmail.com\"]}]";

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductPublisherService productPublisherService;

  @InjectMocks
  private ProductPublisherBean productPublisherBean;

  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    return productDetailResponse;
  }

  private ProductCollection generateProductCollection() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productCollection.setRestrictedKeywordsDetected(RESTRICTED_KEYWORDS_FIELD);
    return productCollection;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductDetailResponse productDetailResponse = this.generateProductDetailResponse();
    ProductCollection productCollection = this.generateProductCollection();
    Mockito.when(
        this.productRepository.findProductDetailByProductCode(eq(ProductPublisherTest.DEFAULT_PRODUCT_CODE_1)))
        .thenReturn(productDetailResponse);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productRepository)
        .findProductDetailByProductCode(eq(ProductPublisherTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.when(
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(),
            eq(DEFAULT_PRODUCT_CODE_1))).thenReturn(productCollection);
    Mockito.doThrow(RuntimeException.class).when(this.productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE_2));
    Mockito.when(
        this.productPublisherService.publish(any(), any(),
            any(), any(), eq(false), eq(false),
            eq(RESTRICTED_KEYWORDS_FIELD), eq(0), eq(false), any(), any())).thenReturn(null);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any()))
      .thenReturn(
        ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).activated(true)
          .trustedSeller(false).markForDelete(false).build());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.productPublisherService);
  }

  @Test
  public void publishTest() throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put("appName", "PDT");
    datas.put("productCode", ProductPublisherTest.DEFAULT_PRODUCT_CODE_1);
    this.productPublisherBean.publish(datas);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), any());
    Mockito.verify(this.productPublisherService).publish(eq(DEFAULT_PRODUCT_CODE_1), eq(BUSINESS_PARTNER_CODE),
        eq(BUSINESS_PARTNER_NAME), any(), eq(false), eq(false),
        eq(RESTRICTED_KEYWORDS_FIELD), eq(0), eq(false), any(), any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
  }

  @Test
  public void publishWithExceptionTest() throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put("appName", "PDT");
    datas.put("productCode", ProductPublisherTest.DEFAULT_PRODUCT_CODE_2);
    this.productPublisherBean.publish(datas);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), eq(DEFAULT_PRODUCT_CODE_2));
  }

  @Test
  public void publishWithUnregisteredAppTest() throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put("appName", "OB");
    datas.put("productCode", ProductPublisherTest.DEFAULT_PRODUCT_CODE_1);
    this.productPublisherBean.publish(datas);
  }

}
