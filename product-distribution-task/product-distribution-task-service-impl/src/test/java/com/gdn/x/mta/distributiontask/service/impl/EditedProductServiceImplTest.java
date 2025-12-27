package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import com.gdn.mta.domain.event.modal.PriceInfoDTO;
import com.gdn.x.productcategorybase.dto.AttributeType;
import org.springframework.beans.BeanUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;
import com.gdn.partners.pdt.service.distribution.DistributionTaskHistoryService;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.dao.api.ProductAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.EditedReviewTypeConstants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.impl.util.ImageUtils;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;
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


public class EditedProductServiceImplTest {

  @InjectMocks
  private EditedProductServiceImpl editedProductService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private DistributionTaskService distributionTaskService;

  @Mock
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Mock
  private VendorRepository vendorRepository;

  @Mock
  private ProductRepository productRepository;


  @Mock
  private ProductDistributionTaskRepository distributionTaskRepository;

  @Mock
  private DistributionTaskHistoryService distributionTaskHistoryService;

  @Mock
  private ProductUtils productUtils;

  @Mock
  private ProductItemRepository productItemRepository;

  @Mock
  private ProductAttributeRepository productAttributeRepository;

  @Mock
  private ProductImageRepository productImageRepository;

  @Mock
  private ProductDistributionTaskService productDistributionTaskService;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private FileStorageService fileStorageService;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> oldProductArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductReviewer> productReviewerArgumentCaptor;

  private static final String STORE_ID = "10001";
  private static final String USER_NAME = "PDT-USER";
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
  private static final String CONTENT_ASSIGNEE = "content assignee";
  private static final String IMAGE_ASSIGNEE = "image assignee";
  private static final String RESTRICTED_KEYWORD = "restrictedKeyword";

  private ProductDetailResponse productDetailResponse;
  private Product existingProduct;
  private AddEditedProductToPDTEvent addEditedProductToPDTEvent;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private RestrictedKeywordsByFieldResponse restrictedKeywordsByFieldResponse;
  private String restrictedKeywordFieldJson;
  private ObjectMapper mapper;
  private ProductReviewer productReviewer;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    ReflectionTestUtils.setField(editedProductService, "autoDistributeSwitch", AUTO_DISTRIBUTE_SWITCH);
    ReflectionTestUtils.setField(editedProductService, "autoDistributeDefaultVendorCode", VENDOR_ID);

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

    addEditedProductToPDTEvent = new AddEditedProductToPDTEvent();
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    addEditedProductToPDTEvent.setProductCode(PRODUCT_CODE);
    addEditedProductToPDTEvent.setMerchantName(BP_NAME);
    addEditedProductToPDTEvent.setMerchantCode(BP_CODE);
    addEditedProductToPDTEvent.setPostLive(true);

    restrictedKeywordsByFieldResponse =
        new RestrictedKeywordsByFieldResponse(RestrictedKeywordFieldNames.PRODUCT_NAME.name(),
            List.of(RESTRICTED_KEYWORD));
    restrictedKeywordFieldJson =
        mapper.writeValueAsString(List.of(restrictedKeywordsByFieldResponse));

    productReviewer = ProductReviewer.builder().build();
    Mockito.when(productReviewerService.findProductReviewerByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    ImageUtils.setFileStorageService(fileStorageService);
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
    Mockito.verifyNoMoreInteractions(productWrapperService);
    Mockito.verifyNoMoreInteractions(distributionTaskService);
    Mockito.verifyNoMoreInteractions(autoDistributionConfigurationRepository);
    Mockito.verifyNoMoreInteractions(vendorRepository);
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(distributionTaskRepository);
    Mockito.verifyNoMoreInteractions(distributionTaskHistoryService);
    Mockito.verifyNoMoreInteractions(productUtils);
    Mockito.verifyNoMoreInteractions(productItemRepository);
    Mockito.verifyNoMoreInteractions(productAttributeRepository);
    Mockito.verifyNoMoreInteractions(productImageRepository);
    Mockito.verifyNoMoreInteractions(productDistributionTaskService);

  }

  @Test
   void editProductDetailsTest() throws Exception {
    PriceInfoDTO priceInfoDTO = new PriceInfoDTO();
    priceInfoDTO.setItemId(ITEM_SKU_CODE1);
    addEditedProductToPDTEvent.setRestrictedKeywordsPresent(true);
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    addEditedProductToPDTEvent.setRestrictedKeywordsDetected(Arrays.asList(restrictedKeywordsByFieldResponse));
    addEditedProductToPDTEvent.setPriceInfo(Collections.singletonList(priceInfoDTO));
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(
        AttributeResponse.builder().attributeType(AttributeType.PREDEFINED_ATTRIBUTE.name())
            .attributeCode(ATTRIBUTE_CODE1).dsExtraction(true).build());
    productAttributeResponse.setProductAttributeValues(Arrays.asList(new ProductAttributeValueResponse()));
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setDsExtraction(false);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    existingProduct.setProductAttributes(Arrays.asList(productAttribute));
    Product updatedProduct = new Product();
    BeanUtils.copyProperties(existingProduct, updatedProduct);
    updatedProduct.setProductAttributes(Arrays.asList(productAttribute));
    updatedProduct.getProductAttributes().get(0).setDsExtraction(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(updatedProduct);
    Product product =
        editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    Assertions.assertTrue(product.getProductAttributes().get(0).isDsExtraction());
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(vendorRepository).findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID);
    Mockito.verify(productService).updateProductNotesForEditedProducts(Mockito.any(Product.class), Mockito.eq(null));
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertEquals(STORE_ID, productArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(STORE_ID, productReviewerArgumentCaptor.getValue().getStoreId());
    Assertions.assertTrue(productArgumentCaptor.getValue().isRestrictedKeywordsPresent());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void editProductDetailsImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(vendorRepository).findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID);
    Mockito.verify(productService).updateProductNotesForEditedProducts(Mockito.any(Product.class), Mockito.eq(null));
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditForTrustedSellerTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    addEditedProductToPDTEvent.setTrustedSeller(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
      thenReturn(productDetailResponse);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(vendorRepository).findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID);
    Mockito.verify(productService).updateProductNotesForEditedProducts(Mockito.any(Product.class), Mockito.any());
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(distributionTaskService)
      .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
        Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }


  @Test
   void editProductDetailsImageEditVendorSwitchTest() throws Exception {
    ReflectionTestUtils.setField(editedProductService, "autoDistributeSwitch", Boolean.TRUE);
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(autoDistributionConfigurationRepository
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(VENDOR_ID);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(vendorRepository).findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID);
    Mockito.verify(productService).updateProductNotesForEditedProducts(Mockito.any(Product.class), Mockito.eq(null));
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(autoDistributionConfigurationRepository)
        .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditInReviewTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditInReview1Test() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditContentApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    addEditedProductToPDTEvent.setRestrictedKeywordsDetected(Arrays.asList(restrictedKeywordsByFieldResponse));
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void editProductDetailsContentEditContentApprovedStateWithPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(ReviewType.IMAGE);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditImageApprovedWithPreviousContentEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.CONTENT);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditImageApprovedWithPreviousContentEdit1Test() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditContentAndImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditContentAndImageApprovedPreviousContentEitTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(ReviewType.CONTENT);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditInReviewTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    addEditedProductToPDTEvent.setRestrictedKeywordsDetected(Arrays.asList(restrictedKeywordsByFieldResponse));
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void editProductDetailsImageEditInReview1Test() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    existingProduct.setEdited(true);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditContentApprovedStateWithPreviousContentEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(ReviewType.CONTENT);
    existingProduct.setEdited(true);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditContentApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(CONTENT_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(CONTENT_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditImageApprovedWithPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.IMAGE);
    existingProduct.setEdited(true);
    productReviewer.setApproverAssignee(CONTENT_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(CONTENT_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditImageApprovedWithPreviousImageEdit1Test() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    existingProduct.setEdited(true);
    productReviewer.setApproverAssignee(CONTENT_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(CONTENT_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditContentAndImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.PASSED);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditContentAndImageApprovedPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.PASSED);
    productReviewer.setApprovedDate(new Date());
    productReviewer.setAssignedDate(new Date());
    existingProduct.setReviewType(ReviewType.IMAGE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentRefreshInReviewTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentRefreshContentApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService).updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentRefreshContentApprovedPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.IMAGE);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentRefreshImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsContentRefreshContentAndImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsContentRefreshPassedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    updatePassedProduct();
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsContentRefreshCpntentAndImageApprovedAnPreviousContentEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setReviewType(ReviewType.CONTENT);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsImageRefreshInReviewTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsImageRefreshImageApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsImageRefreshImageApprovedPreviousContentEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(ReviewType.CONTENT);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsWithNullProductReviewerTest() throws Exception {
    addEditedProductToPDTEvent.setRestrictedKeywordsPresent(true);
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    addEditedProductToPDTEvent.setRestrictedKeywordsDetected(Arrays.asList(restrictedKeywordsByFieldResponse));
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productReviewerService.addNewProduct(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
      .thenReturn(ProductReviewer.builder().productCode(PRODUCT_CODE).build());
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
      thenReturn(productDetailResponse);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
      productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
  }

  @Test
   void editProductDetailsImageRefreshContentApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertEquals(IMAGE_ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsImageRefreshContentAndContentApprovedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setReviewType(null);
    productReviewer.setApproverAssignee(IMAGE_ASSIGNEE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsImageRefreshPassedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    updatePassedProduct();
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsImageRefreshContentAndImageApprovedAnPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setReviewType(ReviewType.IMAGE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
  }

  @Test
   void editProductDetailsConfigChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    addEditedProductToPDTEvent.setTrustedSeller(false);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setForceReview(true);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setReviewType(ReviewType.IMAGE);
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigChangeForTrustedSeller() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    addEditedProductToPDTEvent.setTrustedSeller(true);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setForceReview(true);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setReviewType(ReviewType.IMAGE);
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
      thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
      .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
      .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }



  @Test
   void editProductDetailsConfigInReviewImageRefreshChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigImageEditPassedChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.PASSED);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigImageEditImageApprovedChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigImageEditContentApprovedChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigImageRefreshContentAndImageApprovedState() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setReviewType(ReviewType.IMAGE);
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigSuccess() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    existingProduct.setReviewType(ReviewType.IMAGE);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditPassedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditPassedTest1() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(null);
    List<ProductItem> productItems = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setProduct(existingProduct);
    productItems.add(productItem);
    existingProduct.setProductItems(productItems);

    List<ProductAttribute> productAttributeList = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttributeList.add(productAttribute);
    productAttribute.setProduct(existingProduct);
    existingProduct.setProductAttributes(productAttributeList);

    List<ProductImage> productImageList = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImageList.add(productImage);
    productImage.setProduct(existingProduct);
    existingProduct.setProductImages(productImageList);

    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditPassedPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(ReviewType.IMAGE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsImageEditPassedPreviousContentEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(ReviewType.CONTENT);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditPassedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditPassedTest1() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(null);
    List<ProductItem> productItems = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setProduct(existingProduct);
    productItems.add(productItem);
    existingProduct.setProductItems(productItems);

    List<ProductAttribute> productAttributeList = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttributeList.add(productAttribute);
    productAttribute.setProduct(existingProduct);
    existingProduct.setProductAttributes(productAttributeList);

    List<ProductImage> productImageList = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImageList.add(productImage);
    productImage.setProduct(existingProduct);
    existingProduct.setProductImages(productImageList);

    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditPassedPreviousContentEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(ReviewType.CONTENT);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditPassedPreviousImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    updatePassedProduct();
    existingProduct.setReviewType(ReviewType.IMAGE);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsContentEditPassedDeletedTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setProduct(existingProduct);
    List<ProductItemAttribute> productAttributeList = new ArrayList<>();
    ProductItemAttribute productAttribute = new ProductItemAttribute();
    productAttributeList.add(productAttribute);
    productAttribute.setProduct(productItem);
    productItem.setProductItemAttributes(productAttributeList);
    List<ProductItemImage> productImageList = new ArrayList<>();
    ProductItemImage productImage = new ProductItemImage();
    productImageList.add(productImage);
    productImage.setProductItem(productItem);
    productItem.setProductItemImages(productImageList);
    existingProduct.setProductItems(Arrays.asList(productItem));
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    updatePassedProduct();
    existingProduct.setMarkForDelete(true);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(productService).updateApprovedProductData(Mockito.any(), Mockito.any());
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertFalse(productReviewerArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
   void editProductDetailsImageEditPassedDeletedTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    updatePassedProduct();
    existingProduct.setMarkForDelete(true);
    existingProduct.setReviewType(null);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(productService).updateApprovedProductData(Mockito.any(), Mockito.any());
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .updateEditedProductImageDetails(oldProductArgumentCaptor.capture(), productArgumentCaptor.capture());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.any(), Mockito.anyList(),
            Mockito.any());
    Mockito.verify(distributionTaskRepository).updateProductDistributionTask(Mockito.anyList());
    Mockito.verify(distributionTaskRepository).saveAll(Mockito.anyList());
    Mockito.verify(distributionTaskHistoryService).create(Mockito.anyList());
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertFalse(productReviewerArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
   void editProductDetailsConfigInReviewContentRefreshChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigContentEditPassedChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.PASSED);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigContentEditImageApprovedChange() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_EDIT);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigContentRefreshContentAndImageApprovedState() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.IN_REVIEW);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setReviewType(ReviewType.CONTENT);
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    editedProductService.editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT, productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void editProductDetailsConfigContentRefreshContentAndImageApprovedState1() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.CONTENT_REFRESH);
    existingProduct.setState(WorkflowState.PASSED);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    existingProduct.setPostLive(true);
    addEditedProductToPDTEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getProductDetailByProductCode(USER_NAME, PRODUCT_CODE)).
        thenReturn(productDetailResponse);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(existingProduct);
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(VENDOR_ID)).thenReturn(generateVendor());
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(existingProduct);
    existingProduct.setCreatedDate(new Date());
    Product product = editedProductService
        .editProductDetails(PRODUCT_CODE, addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(USER_NAME, PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productWrapperService).updateEditedProductDetails(oldProductArgumentCaptor.capture(),
        productArgumentCaptor.capture(), Mockito.eq(addEditedProductToPDTEvent.getAllModifiedFields()));
    Mockito.verify(productDistributionTaskService).findByProductId(existingProduct.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(Mockito.any(), Mockito.eq(WorkflowState.IN_REVIEW));
    Mockito.verify(productReviewerService).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE,
        productArgumentCaptor.getValue().getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productArgumentCaptor.getValue().getState());
    Assertions.assertTrue(productArgumentCaptor.getValue().isEdited());
    Assertions.assertFalse(oldProductArgumentCaptor.getValue().isPostLive());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNotNull(product.getCreatedDate());
  }

  private void updatePassedProduct() {
    existingProduct.setState(WorkflowState.PASSED);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    productReviewer.setMarkForDelete(true);
  }

  @Test
   void testInit() {
    editedProductService.init();
  }
}
