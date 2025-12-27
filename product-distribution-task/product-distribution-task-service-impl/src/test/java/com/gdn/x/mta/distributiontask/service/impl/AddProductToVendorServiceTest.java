package com.gdn.x.mta.distributiontask.service.impl;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.modal.AutoApprovalTypeRequestModel;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadge;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import org.apache.commons.lang3.StringUtils;
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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.config.BeanConfiguration;
import com.gdn.x.mta.distributiontask.inbound.util.ProductDomainEventModelConverterUtils;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.EditedReviewTypeConstants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.type.ProductLabels;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.service.api.EditedProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.RevisedProductService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.productcategorybase.AttributeType;
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

public class AddProductToVendorServiceTest {
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
  private static final String UPDATED_BY = "UPDATED_BY";
  private static final String DESCRIPTION = "DESCRIPTION";
  private static final String BLUR_PREDICTION = "blur";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "Approved";
  private static final String DEFINING_ATTRIBUTE = "DEFINING_ATTRIBUTE";
  private static final String PREDEFINED_ATTRIBUTE = "PREDEFINED_ATTRIBUTE";
  private static final String DESCRIPTIVE_ATTRIBUTE = "DESCRIPTIVE_ATTRIBUTE";
  private static final String IMAGE_PREDICTION_RESPONSE_1 =
      "[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":true,\"confidence\":80}]}]";
  private static final String RESTRICTED_KEYWORD = "restrictedKeyword";
  private static final String STORE_ID = "10001";

  @InjectMocks
  private AddProductToVendorServiceImpl addProductToVendorService;

  @Mock
  private DistributionTaskService distributionTaskService;

  @Mock
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private EditedProductService editedProductService;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private RevisedProductService revisedProductService;

  @Mock
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductImageQcFeedbackRequest> productImageQcFeedbackRequestArgumentCaptor;

  private final ObjectMapper mapper = new ObjectMapper();
  private ScreeningProductApprovalEvent screeningProductApprovalEvent;
  private ProductDetailResponse productDetailResponse;
  private ImageQcProcessedResponse imageQcProcessedResponse;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private AddEditedProductToPDTEvent addEditedProductToPDTEvent;
  private ProductReviewer productReviewer;
  private Product product;
  private AddRevisedProductToPDTEvent addRevisedProductToPDTEvent;
  private ProductAndReviewerDetailsDTO productAndReviewerDetails;
  private String restrictedKeywordFieldJson;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);

    RestrictedKeywordsByFieldResponse restrictedKeywordsByFieldResponse =
        new RestrictedKeywordsByFieldResponse(RestrictedKeywordFieldNames.PRODUCT_NAME.name(),
            List.of(RESTRICTED_KEYWORD));
    screeningProductApprovalEvent =
        ScreeningProductApprovalEvent.builder().productCode(PRODUCT_CODE).merchantCode(BP_CODE).merchantName(BP_NAME)
            .updatedBy(UPDATED_BY).postLive(Boolean.TRUE)
            .restrictedKeywordsDetected(List.of(restrictedKeywordsByFieldResponse)).b2bActivated(false)
            .b2cActivated(true).build();

    restrictedKeywordFieldJson =
        mapper.writeValueAsString(List.of(restrictedKeywordsByFieldResponse));

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setForReview(true);
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setDescription(PRODUCT_DESCRIPTION.getBytes());

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

    productDetailResponse.setProductCategoryResponses(
        Arrays.asList(productCategoryResponse2, productCategoryResponse1));

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setMarkForDelete(false);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    attributeResponse1.setAttributeType(DEFINING_ATTRIBUTE);
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
    attributeResponse2.setAttributeType(DESCRIPTIVE_ATTRIBUTE);
    productAttributeResponse2.setAttribute(attributeResponse2);
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setDescriptiveAttributeValue(DESCRIPTION);
    productAttributeValueResponse2.setMarkForDelete(false);
    productAttributeResponse2.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse2));

    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setMarkForDelete(false);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE3);
    attributeResponse3.setAttributeType(PREDEFINED_ATTRIBUTE);
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
    attributeResponse5.setAttributeType(DEFINING_ATTRIBUTE);
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

    productDetailResponse.setProductAttributeResponses(
        Arrays.asList(productAttributeResponse1, productAttributeResponse2, productAttributeResponse3,
            productAttributeResponse4, productAttributeResponse5, productAttributeResponse6, productAttributeResponse7));

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
    productItemResponse2.setProductItemAttributeValueResponses(
        Collections.singletonList(productItemAttributeValueResponse3));

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

    Mockito.when(distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE))
        .thenReturn(productDetailResponse);

    imageQcProcessedResponse = new ImageQcProcessedResponse();
    imageQcProcessedResponse.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponse.setProductPredictionScore(10);
    imageQcProcessedResponse.setImageQcResponse(IMAGE_PREDICTION_RESPONSE_1);
    imageQcProcessedResponse.setImageViolations(BLUR_PREDICTION);

    imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(imageQcProcessedResponse);
    imageQcProcessedAndBrandResponse.setBrandCode(BRAND_CODE);
    imageQcProcessedAndBrandResponse.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);

    productReviewer = ProductReviewer.builder().build();

    addEditedProductToPDTEvent =
        AddEditedProductToPDTEvent.builder().merchantCode(BP_CODE).merchantName(BP_NAME).productCode(PRODUCT_CODE)
            .reviewTypes(EditedReviewTypeConstants.CONTENT_EDIT).postLive(true).build();

    addRevisedProductToPDTEvent =
        AddRevisedProductToPDTEvent.builder().merchantCode(BP_CODE).merchantName(BP_NAME).productCode(PRODUCT_CODE)
            .postLive(true).build();

    product = new Product();
    product.setProductCode(PRODUCT_CODE);

    productAndReviewerDetails = new ProductAndReviewerDetailsDTO();
    productAndReviewerDetails.setProduct(product);
    productAndReviewerDetails.setProductReviewer(productReviewer);

    Mockito.when(this.productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
  }


  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(distributionTaskService);
    Mockito.verifyNoMoreInteractions(solrVendorCollectionService);
    Mockito.verifyNoMoreInteractions(editedProductService);
    Mockito.verifyNoMoreInteractions(productWrapperService);
    Mockito.verifyNoMoreInteractions(revisedProductService);
    Mockito.verifyNoMoreInteractions(approvedProductPublisherService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productServiceRepository);
  }

  @Test
   void processScreeningApprovalEventTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
        Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Assertions.assertEquals(productArgumentCaptor.getValue()
      .getSellerType(), SellerType.NON_TRUSTED_SELLER);
  }

  @Test
  void processScreeningApprovalEventTest_withAiGeneratedFields() throws Exception {
    productDetailResponse.setAiGeneratedFieldsResponse(new AiGeneratedFieldsResponse(true, true));
    screeningProductApprovalEvent.setProductCreationType(
        ProductCreationType.CONVERTED_BULK_UPLOAD.getProductCreationType());
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
        Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Assertions.assertEquals(productArgumentCaptor.getValue()
        .getSellerType(), SellerType.NON_TRUSTED_SELLER);
    Assertions.assertEquals(ProductCreationType.CONVERTED_BULK_UPLOAD.getProductCreationType(),
        productArgumentCaptor.getValue().getProductCreationType());
    Assertions.assertTrue(
        StringUtils.isNoneBlank(productArgumentCaptor.getValue().getAiGeneratedFields()));
  }

  private void setDuplicateProductAttributeInProductDetailResponse(ProductDetailResponse productDetailResponse) {
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setMarkForDelete(false);
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE1);
    attributeResponse2.setAttributeType(DEFINING_ATTRIBUTE);
    productAttributeResponse2.setAttribute(attributeResponse2);
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setDescriptiveAttributeValue(ATTRIBUTE_VALUE1);
    productAttributeValueResponse2.setMarkForDelete(false);
    productAttributeResponse2.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse2));

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setMarkForDelete(false);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    attributeResponse1.setAttributeType(PREDEFINED_ATTRIBUTE);
    productAttributeResponse1.setAttribute(attributeResponse1);
    ProductAttributeValueResponse productAttributeValueResponse3 = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE2);
    predefinedAllowedAttributeValueResponse.setValue(ATTRIBUTE_VALUE2);
    productAttributeValueResponse3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse3.setMarkForDelete(false);
    productAttributeResponse1.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse3));

    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse2,
      productAttributeResponse1));
  }

  private void setDuplicateProductItemAttributes(ProductDetailResponse productDetailResponse){
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
    itemAttributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    itemAttributeResponse1.setAttributeType(ATTRIBUTE_TYPE1);
    productItemAttributeValueResponse1.setAttributeResponse(itemAttributeResponse1);
    ProductItemAttributeValueResponse productItemAttributeValueResponse2 = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse2.setMarkForDelete(true);
    AttributeResponse itemAttributeResponse2 = new AttributeResponse();
    itemAttributeResponse2.setAttributeCode(ATTRIBUTE_CODE1);
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
    itemAttributeResponse3.setAttributeCode(ATTRIBUTE_CODE1);
    productItemAttributeValueResponse3.setAttributeResponse(itemAttributeResponse2);
    productItemResponse2.setProductItemAttributeValueResponses(
      Collections.singletonList(productItemAttributeValueResponse3));

    productDetailResponse.setProductItemResponses(new HashSet<>());
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
  }


  @Test
   void processScreeningApprovalEventWithDuplicateAttributeTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    setDuplicateProductAttributeInProductDetailResponse(productDetailResponse);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
      Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Assertions.assertEquals(productArgumentCaptor.getValue()
      .getSellerType(), SellerType.NON_TRUSTED_SELLER);
  }

  @Test
   void processScreeningApprovalEventWithDuplicateItemAttributeTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    setDuplicateProductItemAttributes(productDetailResponse);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
      Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Assertions.assertEquals(productArgumentCaptor.getValue()
      .getSellerType(), SellerType.NON_TRUSTED_SELLER);
  }

  @Test
   void processScreeningApprovalEvent_forTrustedSellersTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    screeningProductApprovalEvent.setTrustedSeller(true);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
      Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(restrictedKeywordFieldJson,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
    Assertions.assertEquals(productArgumentCaptor.getValue()
      .getSellerType(), SellerType.TRUSTED_SELLER);
    Assertions.assertFalse(productArgumentCaptor.getValue().isForceReview());
  }

  @Test
   void processScreeningApprovalEventImageTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    productDetailResponse.getImages().get(1).setMarkForDelete(false);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(2, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertNull(
        productArgumentCaptor.getValue().getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
  }

  @Test
   void processScreeningApprovalEventImageResponseTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    productDetailResponse.getImages().get(1).setMarkForDelete(false);
    screeningProductApprovalEvent.setImageQcCheck(true);
    this.screeningProductApprovalEvent.setSellerBadge(SellerBadgeConstants.GOLD_MERCHANT.getValue());
    getProduct(imageQcProcessedAndBrandResponse, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(2, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertNull(
        productArgumentCaptor.getValue().getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(BRAND_CODE, productArgumentCaptor.getValue().getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS,
        productArgumentCaptor.getValue().getBrandApprovalStatus());
    Assertions.assertEquals(BLUR_PREDICTION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(SellerBadge.GOLD_MERCHANT,
        productArgumentCaptor.getValue().getSellerBadge());
  }

  @Test
   void processScreeningApprovalEventResponseEmptyImageViolationTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    imageQcProcessedResponse.setImageViolations(StringUtils.EMPTY);
    productDetailResponse.getImages().get(1).setMarkForDelete(false);
    screeningProductApprovalEvent.setImageQcCheck(true);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(2, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertNull(
        productArgumentCaptor.getValue().getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertEquals(ProductLabels.PENDING.getDescription(),
        productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(0, productArgumentCaptor.getValue().getProductPredictionScore());
  }

  @Test
   void processScreeningApprovalEventWithNoCategoryTest() throws Exception {
    productDetailResponse.getProductCategoryResponses()
      .forEach(productCategoryResponse -> productCategoryResponse.setMarkForDelete(true));
    Assertions.assertThrows(Exception.class,
      () -> addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent,
        0));
    Mockito.verify(distributionTaskService)
      .getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
  }

  @Test
   void processScreeningApprovalEventWithNoItemAttributeValueTest() throws Exception {
    productDetailResponse.getProductItemResponses()
        .forEach(productItemResponse -> productItemResponse.setProductItemAttributeValueResponses(null));
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Assertions.assertEquals(0,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
  }

  @Test
   void processScreeningApprovalEventWithExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(distributionTaskService)
      .autoDistribute(Mockito.any(Product.class), Mockito.eq(true));
    Assertions.assertThrows(Exception.class,
      () -> addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent,
        0));
    Mockito.verify(distributionTaskService)
      .getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService)
      .autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);

  }

  @Test
   void processScreeningApprovalEventPriority1WithExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(distributionTaskService)
      .autoDistribute(Mockito.any(Product.class), Mockito.eq(true));
    Assertions.assertThrows(Exception.class,
      () -> addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent,
        1));
    Mockito.verify(distributionTaskService)
      .getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService)
      .autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
  }

  @Test
   void processScreeningApprovalEventPriority2WithExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(distributionTaskService)
        .autoDistribute(Mockito.any(Product.class), Mockito.eq(true));
    try {
      Assertions.assertThrows(Exception.class,
        () -> addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 2));
    } finally {
      Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
      Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
      Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    }
  }

  @Test
   void processScreeningApprovalEvent_preLive() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    screeningProductApprovalEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    getProduct(imageQcProcessedAndBrandResponse, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
        Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertFalse(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(productArgumentCaptor.getValue().getSellerType(),
        SellerType.NON_TRUSTED_SELLER);

  }

  @Test
   void processScreeningApprovalEvent_preLive_forTrustedSellersTest() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    screeningProductApprovalEvent.setPostLive(false);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    screeningProductApprovalEvent.setTrustedSeller(true);
    getProduct(imageQcProcessedAndBrandResponse, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 0);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(),
      Mockito.eq(true));
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE9,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(3).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(3).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE10,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        productArgumentCaptor.getValue().getProductAttributes().get(4).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(4).getAttributeType());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertFalse(productArgumentCaptor.getValue().isPostLive());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPromoSKU());
    Assertions.assertEquals(productArgumentCaptor.getValue().getSellerType(),
        SellerType.TRUSTED_SELLER);
  }

  @Test
   void processScreeningApprovalEventPriority1Test() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(null);
    productDetailResponse.getImages().get(1).setMarkForDelete(false);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 1);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getPrioritySeller());
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(2, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertNull(
        productArgumentCaptor.getValue().getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
  }

  @Test
   void processScreeningApprovalEventPriority2Test() throws Exception {
    ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponse.getImageQcResponse());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(null);
    productDetailResponse.getImages().get(1).setMarkForDelete(false);
    getProduct(null, screeningProductApprovalEvent);
    addProductToVendorService.processScreeningApprovalEvent(screeningProductApprovalEvent, 2);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).getProductDetailByProductCode(BeanConfiguration.USER_NAME, PRODUCT_CODE);
    Mockito.verify(distributionTaskService).autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true));
    Assertions.assertEquals(2, productArgumentCaptor.getValue().getPrioritySeller());
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(distributionTaskService).publishVendorApprovedEvent(productArgumentCaptor.capture(), Mockito.eq(true));
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getDescription()));
    Assertions.assertEquals(PRODUCT_DESCRIPTION,
        new String(productArgumentCaptor.getValue().getLongDescription()));
    Assertions.assertEquals(CATEGORY_CODE1, productArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, productArgumentCaptor.getValue().getCategoryName());
    Assertions.assertEquals(5, productArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE1,
        productArgumentCaptor.getValue().getProductAttributes().get(0).getValue());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE2,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeCode());
    Assertions.assertEquals(DESCRIPTION,
        productArgumentCaptor.getValue().getProductAttributes().get(1).getValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(1).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_CODE3,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE2,
        productArgumentCaptor.getValue().getProductAttributes().get(2).getValue());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE.name(),
        productArgumentCaptor.getValue().getProductAttributes().get(2).getAttributeType());
    Assertions.assertEquals(2, productArgumentCaptor.getValue().getProductImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        productArgumentCaptor.getValue().getProductImages().get(0).getLocationPath());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getProductItems().size());
    Assertions.assertEquals(ITEM_SKU_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getSkuCode());
    Assertions.assertEquals(ITEM_NAME1,
        productArgumentCaptor.getValue().getProductItems().get(0).getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getUpcCode());
    Assertions.assertEquals(DANGEROUS_GOOD_LEVEL1,
        productArgumentCaptor.getValue().getProductItems().get(0).getDangerousGoodsLevel());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductImages().get(0).getOriginalImage());
    Assertions.assertNull(
        productArgumentCaptor.getValue().getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertEquals(1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_CODE6,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE1,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getAttributeType());
    Assertions.assertEquals(ATTRIBUTE_VALUE4,
        productArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributes().get(0).getValue());
    Assertions.assertTrue(productArgumentCaptor.getValue().isPostLive());
  }

  @Test
   void processAddEditedProductEventTest() throws Exception {
    addRevisedProductToPDTEvent.setSellerBadge(SellerBadgeConstants.SILVER_MERCHANT.name());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(editedProductService
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
        STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(editedProductService)
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  @Test
   void processAddEditedProductEventTest_forTrustedSeller() throws Exception {
    this.addEditedProductToPDTEvent.setTrustedSeller(true);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(editedProductService
      .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
        imageQcProcessedAndBrandResponse)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
      STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(editedProductService)
      .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
        imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }


  @Test
   void processAddEditedProductEventImageQcTest() throws Exception {
    addEditedProductToPDTEvent.setOnlyImageQcDataUpdate(true);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(productWrapperService)
        .updateImageQcResponseByProductCode(addEditedProductToPDTEvent.getImageQcProcessedResponseDomainEvent());
  }

  @Test
   void processAddEditedProductEventImageEditTest() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    addEditedProductToPDTEvent.setSellerBadge(SellerBadgeConstants.SILVER_MERCHANT.getValue());
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
        STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    Mockito.when(editedProductService
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse)).thenReturn(product);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(editedProductService)
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
   void processAddEditedProductEventImageEditTest_forTrustedSeller() throws Exception {
    addEditedProductToPDTEvent.setReviewTypes(EditedReviewTypeConstants.IMAGE_EDIT);
    this.addEditedProductToPDTEvent.setTrustedSeller(true);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
      STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    Mockito.when(editedProductService
      .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
        imageQcProcessedAndBrandResponse)).thenReturn(product);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(editedProductService)
      .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
        imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService)
      .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
        Mockito.eq(true));
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
   void processAddEditedProductEventExceptionTest() throws Exception {
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
        STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    Mockito.doThrow(Exception.class).when(editedProductService)
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent));
    } finally {
      Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
      Mockito.verify(editedProductService)
          .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
              imageQcProcessedAndBrandResponse);
    }
  }

  @Test
   void processAddEditedProductEventImageQcResponseNullTest() throws Exception {
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE)).thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(editedProductService
            .editProductDetails(addEditedProductToPDTEvent.getProductCode(),
                addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
        STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(editedProductService)
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(),
            addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  @Test
   void processAddEditedProductEventImageQcResponseNull1Test() throws Exception {
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(null);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(
        STORE_ID, product.getProductCode())).thenReturn(productReviewer);
    Mockito.when(editedProductService
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse)).thenReturn(product);
    addProductToVendorService.processAddEditedProductEvent(addEditedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(editedProductService)
        .editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  @Test
   void processAddRevisedProductEventTest() throws Exception {
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
            revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  @Test
   void processAddRevisedProductEventNoEligibleForEditTest() throws Exception {
    productAndReviewerDetails.getProduct().setReviewType(ReviewType.CONTENT);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
            revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  @Test
   void processAddRevisedProductEvent_ProductCodeNullTest() throws Exception {
    addRevisedProductToPDTEvent.setProductCode(null);
    String message = mapper.writeValueAsString(addRevisedProductToPDTEvent);
    Assertions.assertThrows(Exception.class,
      () -> addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent));
  }

  @Test
   void processAddRevisedProductEventWithImageQCResponseTest() throws Exception {
    product.setReviewType(ReviewType.IMAGE);
    addRevisedProductToPDTEvent.setTrustedSeller(false);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
            revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    product.setPostLive(true);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(true));
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertFalse(product.isForceReview());
  }

  @Test
   void processAddRevisedProductEventImageApprovedTest() throws Exception {
    addRevisedProductToPDTEvent.setTrustedSeller(false);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
            revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    product.setPostLive(true);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(approvedProductPublisherService)
        .publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(true));
  }

  @Test
   void processAddRevisedProductEventImageApprovedForTrustedSellersTest() throws Exception {
    addRevisedProductToPDTEvent.setTrustedSeller(true);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
        revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
      .thenReturn(productAndReviewerDetails);
    product.setPostLive(true);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
      .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(approvedProductPublisherService)
      .publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(true));
  }

  @Test
   void processAddRevisedProductEventWithImageQCResponseForTrustedSellersTest() throws Exception {
    product.setReviewType(ReviewType.IMAGE);
    addRevisedProductToPDTEvent.setTrustedSeller(true);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
      .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
        revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
      .thenReturn(productAndReviewerDetails);
    product.setPostLive(true);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
      .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(productImageQcFeedbackService)
      .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
        Mockito.eq(true));
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(true));
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertFalse(product.isForceReview());
  }



  @Test
   void processAddRevisedProductEventWithImageQCResponseNullTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    String message = mapper.writeValueAsString(addRevisedProductToPDTEvent);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(null);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
            revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    product.setPostLive(true);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(true));
  }

  @Test
   void processAddRevisedProductEventExceptionTest() throws Exception {
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.doThrow(Exception.class).when(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent));
    } finally {
      Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
      Mockito.verify(revisedProductService)
          .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    }
  }

  @Test
   void onDomainEventConsumedImageQcResponseNullTest() throws Exception {
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService).addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  @Test
   void onDomainEventConsumedImageQcResponseNull1Test() throws Exception {
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(null);
    Mockito.when(distributionTaskService.getImageQcResponseByProductCode(PRODUCT_CODE))
        .thenReturn(imageQcProcessedAndBrandResponse);
    Mockito.when(
            revisedProductService.addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse))
        .thenReturn(productAndReviewerDetails);
    addProductToVendorService.processAddRevisedProductEvent(addRevisedProductToPDTEvent);
    Mockito.verify(distributionTaskService).getImageQcResponseByProductCode(PRODUCT_CODE);
    Mockito.verify(revisedProductService)
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    Mockito.verify(solrVendorCollectionService).publishSolrAddPDTProductBatchEvent(Mockito.any());
  }

  private void getProduct(ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse,
      ScreeningProductApprovalEvent screeningProductApprovalEvent) throws Exception {
    Product product = ProductDomainEventModelConverterUtils
        .convertProductDomainEventModelToProduct(productDetailResponse, this.screeningProductApprovalEvent,
          false, imageQcProcessedAndBrandResponse);
    Mockito.when(distributionTaskService.autoDistribute(productArgumentCaptor.capture(), Mockito.eq(true)))
        .thenReturn(product);
  }

  @Test
   void isEligibleForRevisedTest() {
    productAndReviewerDetails.getProduct().setReviewType(ReviewType.IMAGE);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(new ImageQcProcessedResponse());
    Assertions.assertTrue(addProductToVendorService.isEligibleForRevised(productAndReviewerDetails, imageQcProcessedAndBrandResponse));

    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(null);
    Assertions.assertFalse(addProductToVendorService.isEligibleForRevised(productAndReviewerDetails, imageQcProcessedAndBrandResponse));

    Assertions.assertFalse(
        addProductToVendorService.isEligibleForRevised(productAndReviewerDetails, null));

    productAndReviewerDetails.getProduct().setReviewType(ReviewType.CONTENT);
    Assertions.assertFalse(addProductToVendorService.isEligibleForRevised(productAndReviewerDetails, imageQcProcessedAndBrandResponse));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelTest() throws IOException {
    addProductToVendorService.processDimensionsUpdateEvent(new PDTDimensionRefreshEventModel());
    Mockito.verify(productService).updateProductDimensionsAndProductTypeAndDgLevel(new PDTDimensionRefreshEventModel());
  }

  @Test
   void processAutoApprovalCheckEventTest() throws Exception {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setAutoApprovalType(Constants.CONTENT_AND_IMAGE);
    response.setValue(autoApprovalTypeResponse);
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    autoApprovalTypeRequestModel.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequestModel.setCategoryCode(CATEGORY_CODE2);
    autoApprovalTypeRequestModel.setReviewType(ReviewType.CONTENT.name());
    autoApprovalTypeRequestModel.setStoreId(STORE_ID);
    autoApprovalTypeRequestModel.setDestinationCategoryCode(CATEGORY_CODE2);
    Mockito.when(productServiceRepository
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any()))
        .thenReturn(response);
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    Mockito.when(productService.autoApproveProduct(PRODUCT_CODE)).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().internalHistoryEventModel(internalHistoryEventModel).build());
    addProductToVendorService.processAutoApprovalCheckEvent(autoApprovalTypeRequestModel);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
    Mockito.verify(productService).autoApproveProduct(Mockito.anyString());
    Mockito.verify(productService).publishInternalHistoryEventForProduct(internalHistoryEventModel);
  }

  @Test
   void processAutoApprovalCheckEventNullTest() throws Exception {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setAutoApprovalType(Constants.CONTENT_AND_IMAGE);
    response.setValue(autoApprovalTypeResponse);
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    autoApprovalTypeRequestModel.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequestModel.setCategoryCode(CATEGORY_CODE2);
    autoApprovalTypeRequestModel.setReviewType(ReviewType.CONTENT.name());
    autoApprovalTypeRequestModel.setStoreId(STORE_ID);
    autoApprovalTypeRequestModel.setDestinationCategoryCode(CATEGORY_CODE2);
    Mockito.when(productServiceRepository
            .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any()))
        .thenReturn(response);
    Mockito.when(productService.autoApproveProduct(PRODUCT_CODE)).thenReturn(
        PublishAndSavedProductAndHistoryModel.builder().build());
    addProductToVendorService.processAutoApprovalCheckEvent(autoApprovalTypeRequestModel);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
    Mockito.verify(productService).autoApproveProduct(Mockito.anyString());
  }

  @Test
   void processAutoApprovalCheckSuccessTest() throws Exception {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setAutoApprovalType(Constants.CONTENT_AND_IMAGE);
    response.setValue(autoApprovalTypeResponse);
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    autoApprovalTypeRequestModel.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequestModel.setCategoryCode(CATEGORY_CODE2);
    autoApprovalTypeRequestModel.setReviewType(ReviewType.CONTENT.name());
    autoApprovalTypeRequestModel.setStoreId(STORE_ID);
    autoApprovalTypeRequestModel.setDestinationCategoryCode(CATEGORY_CODE2);
    Mockito.when(productServiceRepository
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any()))
        .thenReturn(response);
    addProductToVendorService.processAutoApprovalCheckEvent(autoApprovalTypeRequestModel);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }
  @Test
   void processAutoApprovalCheckExceptionTest() throws Exception {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setAutoApprovalType(Constants.CONTENT_AND_IMAGE);
    response.setValue(autoApprovalTypeResponse);
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    autoApprovalTypeRequestModel.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequestModel.setCategoryCode(CATEGORY_CODE2);
    autoApprovalTypeRequestModel.setReviewType(ReviewType.CONTENT.name());
    autoApprovalTypeRequestModel.setStoreId(STORE_ID);
    autoApprovalTypeRequestModel.setDestinationCategoryCode(CATEGORY_CODE2);
    Mockito.doThrow(new ApplicationRuntimeException()).when(productServiceRepository)
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
    try {
      addProductToVendorService.processAutoApprovalCheckEvent(autoApprovalTypeRequestModel);
    } finally {
      Mockito.verify(productServiceRepository)
          .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
    }
  }


  @Test
   void processAutoApprovalCheckWithDifferentAutoApprovalTypeTest() throws Exception {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    autoApprovalTypeResponse.setAutoApprovalType(Constants.CONTENT_AUTO_APPROVAL);
    response.setValue(autoApprovalTypeResponse);
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    autoApprovalTypeRequestModel.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequestModel.setCategoryCode(CATEGORY_CODE2);
    autoApprovalTypeRequestModel.setReviewType(ReviewType.CONTENT.name());
    autoApprovalTypeRequestModel.setStoreId(STORE_ID);
    autoApprovalTypeRequestModel.setDestinationCategoryCode(CATEGORY_CODE2);
    Mockito.when(productServiceRepository
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any()))
        .thenReturn(response);
    addProductToVendorService.processAutoApprovalCheckEvent(autoApprovalTypeRequestModel);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
   void processAutoApprovalCheckEventAndNullAutoApproveResponseTest() throws Exception {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    autoApprovalTypeRequestModel.setProductCode(PRODUCT_CODE);
    autoApprovalTypeRequestModel.setCategoryCode(CATEGORY_CODE2);
    autoApprovalTypeRequestModel.setReviewType(ReviewType.CONTENT.name());
    autoApprovalTypeRequestModel.setStoreId(STORE_ID);
    autoApprovalTypeRequestModel.setDestinationCategoryCode(CATEGORY_CODE2);
    Mockito.when(productServiceRepository
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any()))
        .thenReturn(response);
    addProductToVendorService.processAutoApprovalCheckEvent(autoApprovalTypeRequestModel);
    Mockito.verify(productServiceRepository)
        .getAutoApprovalType(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }
}
