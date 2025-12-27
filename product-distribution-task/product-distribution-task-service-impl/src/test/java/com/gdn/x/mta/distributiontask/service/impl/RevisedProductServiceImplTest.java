package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadge;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import com.gdn.x.mta.distributiontask.service.api.AppealProductService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;
import com.gdn.partners.pdt.service.distribution.DistributionTaskHistoryService;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

@ExtendWith(MockitoExtension.class)
public class RevisedProductServiceImplTest {

  @InjectMocks
  private RevisedProductServiceImpl revisedProductService;

  @Mock
  private ProductService productService;

  @Mock
  private DistributionTaskService distributionTaskService;

  @Mock
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Mock
  private VendorService vendorService;

  @Mock
  private ProductDistributionTaskRepository distributionTaskRepository;

  @Mock
  private DistributionTaskHistoryService distributionTaskHistoryService;

  @Mock
  private ProductDistributionTaskService productDistributionTaskService;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private AppealProductService appealProductService;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> oldProductArgumentCaptor;

  private static final String STORE_ID = "10001";
  private static final String USER_NAME = "PDT-USER";
  private static final String PRODUCT_ID = "PRODUCT_ID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String PRODUCT_DESCRIPTION = "PRODUCT_DESCRIPTION";
  private static final String CATEGORY_CODE1 = "CATEGORY_CODE1";
  private static final String CATEGORY_CODE2 = "CATEGORY_CODE2";
  private static final String CATEGORY_NAME1 = "CATEGORY_NAME1";
  private static final String CATEGORY_NAME2 = "CATEGORY_NAME2";
  private static final String ATTRIBUTE_CODE1 = "ATTRIBUTE_CODE1";
  private static final String ATTRIBUTE_CODE2 = "ATTRIBUTE_CODE2";
  private static final String ATTRIBUTE_CODE3 = "ATTRIBUTE_CODE3";
  private static final String ATTRIBUTE_CODE4 = "ATTRIBUTE_CODE4";
  private static final String ATTRIBUTE_CODE5 = "ATTRIBUTE_CODE5";
  private static final String ATTRIBUTE_CODE6 = "ATTRIBUTE_CODE6";
  private static final String ATTRIBUTE_CODE7 = "ATTRIBUTE_CODE7";
  private static final String ATTRIBUTE_CODE8 = "ATTRIBUTE_CODE8";
  private static final String ATTRIBUTE_CODE9 = "ATTRIBUTE_CODE9";
  private static final String ATTRIBUTE_CODE10 = "ATTRIBUTE_CODE10";
  private static final String ATTRIBUTE_VALUE1 = "ATTRIBUTE_VALUE1";
  private static final String ATTRIBUTE_VALUE2 = "ATTRIBUTE_VALUE2";
  private static final String ATTRIBUTE_VALUE3 = "ATTRIBUTE_VALUE3";
  private static final String ATTRIBUTE_VALUE4 = "ATTRIBUTE_VALUE4";
  private static final String ATTRIBUTE_TYPE1 = "ATTRIBUTE_TYPE1";
  private static final String LOCATION_PATH1 = "LOCATION_PATH1";
  private static final String LOCATION_PATH2 = "LOCATION_PATH2";
  private static final String LOCATION_PATH3 = "LOCATION_PATH3";
  private static final String LOCATION_PATH4 = "LOCATION_PATH4";
  private static final String LOCATION_PATH5 = "LOCATION_PATH5";
  private static final String ITEM_SKU_CODE1 = "ITEM_SKU_CODE1";
  private static final String ITEM_NAME1 = "ITEM_NAME1";
  private static final String UPC_CODE1 = "UPC_CODE1";
  private static final Integer DANGEROUS_GOOD_LEVEL1 = 0;
  private static final String BP_CODE = "BP_CODE";
  private static final String BP_NAME = "BP_NAME";
  private static final String DESCRIPTION = "DESCRIPTION";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "Approved";
  private static final String IMAGE_VIOLATION = "IMAGE_VIOLATION";
  private static final int PREDICTION_SCORE = 1;
  private static final String CREATED_DATE = "DATE";
  private static final String VENDOR_ID = "vendorId";
  private static final Boolean AUTO_DISTRIBUTE_SWITCH = Boolean.FALSE;
  private static final String RESTRICTED_KEYWORD = "restrictedKeyword";

  private ProductDetailResponse productDetailResponse;
  private Product existingProduct;
  private AddRevisedProductToPDTEvent addRevisedProductToPDTEvent;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private RestrictedKeywordsByFieldResponse restrictedKeywordsByFieldResponse;
  private String restrictedKeywordFieldJson;
  private ObjectMapper mapper;
  private ProductReviewer productReviewer;
  private AppealedProduct appealedProduct;


  @BeforeEach
  public void setUp() throws Exception {
    ReflectionTestUtils.setField(revisedProductService, "autoDistributeSwitch", AUTO_DISTRIBUTE_SWITCH);
    ReflectionTestUtils.setField(revisedProductService, "autoDistributeDefaultVendorCode", VENDOR_ID);

    mapper = new ObjectMapper();

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setForReview(true);
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setDescription(PRODUCT_DESCRIPTION.getBytes());
    productDetailResponse.setCreatedBy(CREATED_DATE);
    productDetailResponse.setStoreId(STORE_ID);

    imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    imageQcProcessedAndBrandResponse.setBrandCode(BRAND_CODE);

    ProductCategoryResponse productCategoryResponse1 = new ProductCategoryResponse();
    CategoryResponse categoryResponse1 = new CategoryResponse();
    productCategoryResponse1.setMarkForDelete(false);
    categoryResponse1.setName(CATEGORY_NAME1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE1);
    productCategoryResponse1.setCategory(categoryResponse1);
    ProductCategoryResponse productCategoryResponse2 = new ProductCategoryResponse();
    CategoryResponse categoryResponse2 = new CategoryResponse();
    productCategoryResponse2.setMarkForDelete(true);
    categoryResponse2.setName(CATEGORY_NAME2);
    categoryResponse2.setCategoryCode(CATEGORY_CODE2);
    productCategoryResponse2.setCategory(categoryResponse2);
    productDetailResponse
        .setProductCategoryResponses(Arrays.asList(productCategoryResponse2, productCategoryResponse1));

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setMarkForDelete(false);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    productAttributeResponse1.setAttribute(attributeResponse1);
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValue = new AllowedAttributeValueResponse();
    allowedAttributeValue.setAllowedAttributeCode(ATTRIBUTE_CODE1);
    allowedAttributeValue.setValue(ATTRIBUTE_VALUE1);
    productAttributeValueResponse1.setAllowedAttributeValue(allowedAttributeValue);
    productAttributeValueResponse1.setMarkForDelete(false);
    productAttributeResponse1.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse1));
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setMarkForDelete(false);
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE2);
    productAttributeResponse2.setAttribute(attributeResponse2);
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setDescriptiveAttributeValue(DESCRIPTION);
    productAttributeValueResponse2.setMarkForDelete(false);
    productAttributeResponse2.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse2));
    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setMarkForDelete(false);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE3);
    productAttributeResponse3.setAttribute(attributeResponse3);
    ProductAttributeValueResponse productAttributeValueResponse3 = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE3);
    predefinedAllowedAttributeValueResponse.setValue(ATTRIBUTE_VALUE2);
    productAttributeValueResponse3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse3.setMarkForDelete(false);
    productAttributeResponse3.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse3));
    ProductAttributeResponse productAttributeResponse4 = new ProductAttributeResponse();
    productAttributeResponse4.setMarkForDelete(false);
    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE4);
    productAttributeResponse4.setAttribute(attributeResponse4);
    ProductAttributeValueResponse productAttributeValueResponse4 = new ProductAttributeValueResponse();
    productAttributeValueResponse4.setMarkForDelete(false);
    productAttributeValueResponse4.setPredefinedAllowedAttributeValue(null);
    productAttributeResponse4.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse4));
    ProductAttributeResponse productAttributeResponse5 = new ProductAttributeResponse();
    productAttributeResponse5.setMarkForDelete(false);
    AttributeResponse attributeResponse5 = new AttributeResponse();
    attributeResponse5.setAttributeCode(ATTRIBUTE_CODE5);
    productAttributeResponse5.setAttribute(attributeResponse5);
    ProductAttributeValueResponse productAttributeValueResponse5 = new ProductAttributeValueResponse();
    productAttributeValueResponse5.setMarkForDelete(true);
    AllowedAttributeValueResponse allowedAttributeValue2 = new AllowedAttributeValueResponse();
    allowedAttributeValue2.setAllowedAttributeCode(ATTRIBUTE_CODE5);
    allowedAttributeValue2.setValue(ATTRIBUTE_VALUE3);
    productAttributeValueResponse5.setAllowedAttributeValue(allowedAttributeValue2);
    productAttributeResponse5.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse5));
    ProductAttributeResponse productAttributeResponse6 = new ProductAttributeResponse();
    productAttributeResponse6.setMarkForDelete(false);
    AttributeResponse attributeResponse6 = new AttributeResponse();
    attributeResponse6.setAttributeCode(ATTRIBUTE_CODE9);
    attributeResponse6.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse6.setAttribute(attributeResponse6);
    ProductAttributeValueResponse productAttributeValueResponse6 = new ProductAttributeValueResponse();
    productAttributeValueResponse6.setMarkForDelete(false);
    productAttributeValueResponse6.setPredefinedAllowedAttributeValue(null);
    productAttributeResponse6.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse6));
    ProductAttributeResponse productAttributeResponse7 = new ProductAttributeResponse();
    productAttributeResponse7.setMarkForDelete(false);
    AttributeResponse attributeResponse7 = new AttributeResponse();
    attributeResponse7.setAttributeCode(ATTRIBUTE_CODE10);
    attributeResponse7.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    productAttributeResponse7.setAttribute(attributeResponse7);
    ProductAttributeValueResponse productAttributeValueResponse7 = new ProductAttributeValueResponse();
    productAttributeValueResponse7.setDescriptiveAttributeValue(null);
    productAttributeValueResponse7.setMarkForDelete(false);
    productAttributeResponse7.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse7));
    productDetailResponse.setProductAttributeResponses(Arrays
        .asList(productAttributeResponse1, productAttributeResponse2, productAttributeResponse3,
            productAttributeResponse4, productAttributeResponse5, productAttributeResponse6,
            productAttributeResponse7));

    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setMarkForDelete(false);
    productItemResponse1.setSkuCode(ITEM_SKU_CODE1);
    productItemResponse1.setGeneratedItemName(ITEM_NAME1);
    productItemResponse1.setUpcCode(UPC_CODE1);
    productItemResponse1.setDangerousGoodsLevel(DANGEROUS_GOOD_LEVEL1);
    Image productItemImage1 = new Image(true, LOCATION_PATH1, 0);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setOriginalImage(Boolean.TRUE);
    Image productItemImage2 = new Image(true, LOCATION_PATH5, 0);
    productItemImage2.setMarkForDelete(true);
    productItemResponse1.setImages(Arrays.asList(productItemImage1, productItemImage2));
    ProductItemAttributeValueResponse productItemAttributeValueResponse1 = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse1.setValue(ATTRIBUTE_VALUE4);
    productItemAttributeValueResponse1.setMarkForDelete(false);
    AttributeResponse itemAttributeResponse1 = new AttributeResponse();
    itemAttributeResponse1.setAttributeCode(ATTRIBUTE_CODE6);
    itemAttributeResponse1.setAttributeType(ATTRIBUTE_TYPE1);
    productItemAttributeValueResponse1.setAttributeResponse(itemAttributeResponse1);
    ProductItemAttributeValueResponse productItemAttributeValueResponse2 = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse2.setMarkForDelete(true);
    AttributeResponse itemAttributeResponse2 = new AttributeResponse();
    itemAttributeResponse2.setAttributeCode(ATTRIBUTE_CODE7);
    productItemAttributeValueResponse2.setAttributeResponse(itemAttributeResponse2);
    productItemResponse1.setProductItemAttributeValueResponses(
        Arrays.asList(productItemAttributeValueResponse1, productItemAttributeValueResponse2));

    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setMarkForDelete(true);
    Image productItemImage3 = new Image(true, LOCATION_PATH2, 0);
    productItemImage3.setOriginalImage(Boolean.TRUE);
    productItemResponse2.setImages(Collections.singletonList(productItemImage3));
    ProductItemAttributeValueResponse productItemAttributeValueResponse3 = new ProductItemAttributeValueResponse();
    AttributeResponse itemAttributeResponse3 = new AttributeResponse();
    itemAttributeResponse3.setAttributeCode(ATTRIBUTE_CODE8);
    productItemAttributeValueResponse3.setAttributeResponse(itemAttributeResponse2);
    productItemResponse2
        .setProductItemAttributeValueResponses(Collections.singletonList(productItemAttributeValueResponse3));

    productDetailResponse.setProductItemResponses(new HashSet<>());
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
    productDetailResponse.setPromoSKU(Boolean.TRUE);

    Image productImage1 = new Image(true, LOCATION_PATH3, 0);
    productImage1.setMarkForDelete(false);
    productImage1.setOriginalImage(Boolean.TRUE);
    Image productImage2 = new Image(true, LOCATION_PATH4, 0);
    productImage2.setMarkForDelete(true);

    productDetailResponse.setImages(Arrays.asList(productImage1, productImage2));
    existingProduct = new Product();
    existingProduct.setBusinessPartnerName(BP_NAME);
    existingProduct.setBusinessPartnerCode(BP_CODE);
    existingProduct.setPostLive(true);
    existingProduct.setRestrictedKeywordsPresent(true);
    existingProduct.setBrandCode(BRAND_CODE);
    existingProduct.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    existingProduct.setImageViolations(IMAGE_VIOLATION);
    existingProduct.setProductPredictionScore(PREDICTION_SCORE);
    existingProduct.setCurrentVendor(generateVendor());
    existingProduct.setStoreId(Constants.DEFAULT_STORE_ID);
    existingProduct.setProductCode(PRODUCT_CODE);
    existingProduct.setId(PRODUCT_ID);

    addRevisedProductToPDTEvent = new AddRevisedProductToPDTEvent();
    addRevisedProductToPDTEvent.setProductCode(PRODUCT_CODE);
    addRevisedProductToPDTEvent.setMerchantName(BP_NAME);
    addRevisedProductToPDTEvent.setMerchantCode(BP_CODE);
    addRevisedProductToPDTEvent.setPostLive(true);

    restrictedKeywordsByFieldResponse =
        new RestrictedKeywordsByFieldResponse(RestrictedKeywordFieldNames.PRODUCT_NAME.name(),
            List.of(RESTRICTED_KEYWORD));
    restrictedKeywordFieldJson = mapper.writeValueAsString(
        List.of(restrictedKeywordsByFieldResponse));

    productReviewer = new ProductReviewer();
    productReviewer.setProductCode(PRODUCT_CODE);

  }

  private Vendor generateVendor() {
    Vendor vendor = new Vendor();
    vendor.setVendorCode(VENDOR_ID);
    vendor.setSlaInDays(1);
    return vendor;
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(distributionTaskService);
    Mockito.verifyNoMoreInteractions(autoDistributionConfigurationRepository);
    Mockito.verifyNoMoreInteractions(vendorService);
    Mockito.verifyNoMoreInteractions(distributionTaskRepository);
    Mockito.verifyNoMoreInteractions(distributionTaskHistoryService);
    Mockito.verifyNoMoreInteractions(productDistributionTaskService);
    Mockito.verifyNoMoreInteractions(productReviewerService);

  }

  @Test
   void addRevisedProductOnSubmitTest() throws Exception {
    ReflectionTestUtils.setField(revisedProductService, "appealProductEnabled", true);
    appealedProduct = new AppealedProduct();
    appealedProduct.setProductCode(PRODUCT_CODE);
    addRevisedProductToPDTEvent.setRestrictedKeywordsPresent(true);
    addRevisedProductToPDTEvent.setRestrictedKeywordsDetected(
        Collections.singletonList(restrictedKeywordsByFieldResponse));
    addRevisedProductToPDTEvent.setAppealedProduct(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(appealProductService.findAppealProductByProductCode(PRODUCT_CODE))
      .thenReturn(appealedProduct);
    Mockito.when(vendorService.findByVendorCode(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productService.createProduct(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productReviewerService.addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(vendorService).findByVendorCode(VENDOR_ID);
    Mockito.verify(productService).createProduct(productArgumentCaptor.capture());
    Mockito.verify(productService).updateProductNotesForRevisedProducts(Mockito.any(Product.class), Mockito.any());
    Mockito.verify(distributionTaskService).generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID),
        Mockito.any(), Mockito.anyList(), Mockito.any());
    Mockito.verify(productReviewerService).addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(appealProductService).findAppealProductByProductCode(PRODUCT_CODE);
    Mockito.verify(appealProductService).upsertAppealProduct(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(STORE_ID, productArgumentCaptor.getValue().getStoreId());
    Assertions.assertTrue(productArgumentCaptor.getValue().isRestrictedKeywordsPresent());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void addRevisedProductOnSubmit_forTrustedSeller() throws Exception {
    ReflectionTestUtils.setField(revisedProductService, "appealProductEnabled", true);
    addRevisedProductToPDTEvent.setRestrictedKeywordsPresent(true);
    addRevisedProductToPDTEvent.setTrustedSeller(true);
    addRevisedProductToPDTEvent.setSellerBadge(SellerBadgeConstants.GOLD_MERCHANT.getValue());
    addRevisedProductToPDTEvent.setRestrictedKeywordsDetected(
        Collections.singletonList(restrictedKeywordsByFieldResponse));
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
      thenReturn(productDetailResponse);
    Mockito.when(vendorService.findByVendorCode(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productService.createProduct(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
      PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productReviewerService.addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(vendorService).findByVendorCode(VENDOR_ID);
    Mockito.verify(productService).createProduct(productArgumentCaptor.capture());
    Mockito.verify(productService).updateProductNotesForRevisedProducts(Mockito.any(Product.class), Mockito.any());
    Mockito.verify(distributionTaskService).generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID),
      Mockito.any(), Mockito.anyList(), Mockito.any());
    Mockito.verify(productReviewerService).addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(STORE_ID, productArgumentCaptor.getValue().getStoreId());
    Assertions.assertTrue(productArgumentCaptor.getValue().isRestrictedKeywordsPresent());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Assertions.assertEquals(SellerBadge.GOLD_MERCHANT,
        productArgumentCaptor.getValue().getSellerBadge());
  }



  @Test
   void addRevisedProductOnSubmitVendorSwitchTest() throws Exception {
    ReflectionTestUtils.setField(revisedProductService, "autoDistributeSwitch", Boolean.TRUE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(vendorService.findByVendorCode(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productService.createProduct(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(autoDistributionConfigurationRepository
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(VENDOR_ID);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productReviewerService.addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(vendorService).findByVendorCode(VENDOR_ID);
    Mockito.verify(productService).createProduct(productArgumentCaptor.capture());
    Mockito.verify(autoDistributionConfigurationRepository)
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(productService).updateProductNotesForRevisedProducts(Mockito.any(Product.class), Mockito.any());
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
  }

  @Test
   void addRevisedProductOnSubmitInReviewTest() throws Exception {
    addRevisedProductToPDTEvent.setRestrictedKeywordsDetected(
        Collections.singletonList(restrictedKeywordsByFieldResponse));
    existingProduct.setState(WorkflowState.NEED_CORRECTION);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void addRevisedProductOnSubmitContentEditInReview1Test() throws Exception {
    existingProduct.setState(WorkflowState.NEED_CORRECTION);
    existingProduct.setReviewType(ReviewType.CONTENT);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
  }

  @Test
   void addRevisedProductOnSubmitContentEditInReviewForTrustedSellerTest() throws Exception {
    ReflectionTestUtils.setField(revisedProductService, "appealProductEnabled", true);
    existingProduct.setState(WorkflowState.NEED_CORRECTION);
    existingProduct.setReviewType(ReviewType.CONTENT);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
      thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
      PRODUCT_CODE)).thenReturn(productReviewer);
    addRevisedProductToPDTEvent.setTrustedSeller(true);
    addRevisedProductToPDTEvent.setAppealedProduct(true);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
      productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(distributionTaskService)
      .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
        Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
      .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Assertions.assertEquals(SellerType.TRUSTED_SELLER,
        productArgumentCaptor.getValue().getSellerType());
  }


  @Test
   void addRevisedProductOnSubmitContentEditInReview1MFDTrueTest() throws Exception {
    existingProduct.setState(WorkflowState.NEED_CORRECTION);
    existingProduct.setReviewType(ReviewType.CONTENT);
    existingProduct.setMarkForDelete(true);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
  }

  @Test
   void addRevisedProductOnSubmitExceptionTest() throws Exception {
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    try {
      Assertions.assertThrows(Exception.class,
        () -> revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent,
          imageQcProcessedAndBrandResponse));
    } finally {
      Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
      Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    }
  }

  @Test
   void addRevisedProductOnSubmitException2Test() throws Exception {
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    existingProduct.setMarkForDelete(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    try {
      Assertions.assertThrows(Exception.class,
        () -> revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent,
          imageQcProcessedAndBrandResponse));
    } finally {
      Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
      Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    }
  }

  @Test
   void addRevisedProductOnSubmitImageNeedCorrectionTest() throws Exception {
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setMarkForDelete(true);
    existingProduct.setReviewType(ReviewType.IMAGE);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
  }

  @Test
   void addRevisedProductOnSubmitNeedCorrectionTest() throws Exception {
    existingProduct.setState(WorkflowState.NEED_CORRECTION);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService).updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
  }

  @Test
   void addRevisedProductOnSubmitNeedCorrectionConfigChangedTest() throws Exception {
    existingProduct.setState(WorkflowState.NEED_CORRECTION);
    existingProduct.setReviewType(null);
    existingProduct.setPostLive(false);
    existingProduct.setCurrentVendor(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, true)).thenReturn(productReviewer);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(existingProduct);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productService).updateRevisedProductData(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addRevisedProductToPDTEvent.getMerchantModifiedFields()));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, true);
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService).updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
  }

  @Test
   void addRevisedProductOnSubmitNullVendorTest() throws Exception {
    addRevisedProductToPDTEvent.setRestrictedKeywordsPresent(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(vendorService.findByVendorCode(VENDOR_ID)).thenReturn(null);
    Mockito.when(productService.createProduct(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(vendorService).findByVendorCode(VENDOR_ID);
    Mockito.verify(productService).createProduct(productArgumentCaptor.capture());
    Mockito.verify(productService).updateProductNotesForRevisedProducts(Mockito.any(Product.class), Mockito.any());
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(STORE_ID, productArgumentCaptor.getValue().getStoreId());
    Assertions.assertTrue(productArgumentCaptor.getValue().isRestrictedKeywordsPresent());
  }

  @Test
   void addRevisedProductOnSubmitNoVendorTest() throws Exception {
    existingProduct.setCurrentVendor(null);
    addRevisedProductToPDTEvent.setRestrictedKeywordsPresent(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(vendorService.findByVendorCode(VENDOR_ID)).thenReturn(null);
    Mockito.when(productService.createProduct(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    Mockito.when(productReviewerService.resetAssignmentData(productReviewer, false)).thenReturn(productReviewer);
    revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(vendorService).findByVendorCode(VENDOR_ID);
    Mockito.verify(productService).createProduct(productArgumentCaptor.capture());
    Mockito.verify(productService).updateProductNotesForRevisedProducts(Mockito.any(Product.class), Mockito.any());
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerService).resetAssignmentData(productReviewer, false);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(STORE_ID, productArgumentCaptor.getValue().getStoreId());
    Assertions.assertTrue(productArgumentCaptor.getValue().isRestrictedKeywordsPresent());
  }
}
