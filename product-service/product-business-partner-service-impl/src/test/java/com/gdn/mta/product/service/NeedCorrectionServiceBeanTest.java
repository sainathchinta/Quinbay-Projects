package com.gdn.mta.product.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.mta.product.util.BeanUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.partners.pbp.distributiontask.PredefinedAttributeAllowedValueService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTItemNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductItemDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;

public class NeedCorrectionServiceBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "TOQ-00000-00000-00001";
  private static final String ITEM_SKU_1 = "TOQ-00000-00000-00002";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String OLD_CATEGORY_CODE = "oldCategoryCode";
  private static final String OLD_CATEGORY_NAME = "oldCategoryName";
  private static final String ITEM_ATTRIBUTE_VALUE = "itemAttributeValue";
  private static final String PRODUCT_ATTRIBUTE_VALUE = "productAttributeValue";


  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_CODE = "brandCode";
  private static final String NEED_CORRECTION_STATE = "NEED_CORRECTION";
  private static final String LOCATION_PATH = "locationPath";
  private static final String VENDOR_NOTES_FIELD = "vendorNotesField";
  private static final String SKU_CODE = "skuCode";
  private static final String ITEM_ID = "itemId";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String ITEM_ID_1 = "itemId1";
  public static final String ATTRIBUTE_CODE = "attributeCode";
  public static final String PREDEFINED_ATTRIBUTE_CODE = "attributeCode1";
  public static final String DEFINING_ATTRIBUTE_CODE = "attributeCode2";
  public static final String NEW_ATTRIBUTE_CODE = "attributeCode3";
  private static final String AUTO_NEED_REVISION_NOTES =
      "This product contains Illegal items/drugs, in-appropriate image and need to be checked by internal team. We'll activate this product after we review the content.";
  private static final String BUSINESS_PARTNER_CODE = "MTA-1234";

  private PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
  private ProductCollection productCollection = new ProductCollection();
  private ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
  private ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
  private ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
  private ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
  private CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
  private ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
  private PDTProductItemDomainEventModel pdtProductItemDomainEventModel = new PDTProductItemDomainEventModel();
  private PDTProductItemDomainEventModel pdtProductItemDomainEventModel1 = new PDTProductItemDomainEventModel();
  private ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel =
      new ProductItemAttributeValueDomainEventModel();
  private ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel1 =
    new ProductItemAttributeValueDomainEventModel();
  private ProductAttributeDomainEventModel descriptiveProductAttributeDomainEventModel =
      new ProductAttributeDomainEventModel();
  private ProductAttributeDomainEventModel newProductAttributeDomainEventModel =
    new ProductAttributeDomainEventModel();
  private ProductAttributeDomainEventModel definingProductAttributeDomainEventModel =
      new ProductAttributeDomainEventModel();
  private ProductAttributeDomainEventModel predefinedProductAttributeDomainEventModel =
      new ProductAttributeDomainEventModel();
  private ProductAttributeValueDomainEventModel descriptiveProductAttributeValueDomainEventModel =
      new ProductAttributeValueDomainEventModel();
  private ProductAttributeValueDomainEventModel newProductAttributeValueDomainEventModel =
    new ProductAttributeValueDomainEventModel();
  private ProductAttributeValueDomainEventModel definingProductAttributeValueDomainEventModel =
      new ProductAttributeValueDomainEventModel();
  private ProductAttributeValueDomainEventModel predefinedProductAttributeValueDomainEventModel =
      new ProductAttributeValueDomainEventModel();
  private PDTProductNotesDomainEventModel pdtProductNotesDomainEventModel = new PDTProductNotesDomainEventModel();
  private PDTItemNotesDomainEventModel pdtItemNotesDomainEventModel = new PDTItemNotesDomainEventModel();
  private ProductAttributeResponse descriptiveProductAttributeResponse =
    new ProductAttributeResponse();
  private ProductAttributeResponse definingProductAttributeResponse =
    new ProductAttributeResponse();
  private ProductAttributeResponse predefinedProductAttributeResponse =
    new ProductAttributeResponse();
  private AttributeResponse descriptiveAttributeResponse = new AttributeResponse();
  private AttributeResponse predefinedAttributeResponse = new AttributeResponse();
  private AttributeResponse definingAttributeResponse = new AttributeResponse();
  private ProductAttributeValueResponse descriptiveProductAttributeValueResponse =
    new ProductAttributeValueResponse();
  private ProductAttributeValueResponse definingProductAttributeValueResponse =
    new ProductAttributeValueResponse();
  private ProductAttributeValueResponse predefinedProductAttributeValueResponse =
    new ProductAttributeValueResponse();
  private ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
  private ProductCategoryResponse oldProductCategoryResponse = new ProductCategoryResponse();
  private ProductDetailResponse productDetailResponse = new ProductDetailResponse();
  private ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
  private ProductItemDetailResponse productItemDetailResponse1 = new ProductItemDetailResponse();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse =
    new ProductItemAttributeValueResponse();
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
    new PredefinedAllowedAttributeValueResponse();
  private AllowedAttributeValueResponse allowedAttributeValueResponse =
    new AllowedAttributeValueResponse();
  private CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
  private ProductBusinessPartnerCounter productBusinessPartnerCounter;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Mock
  private ProductAppealService productAppealService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private PredefinedAttributeAllowedValueService predefinedAttributeAllowedValueService;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Captor
  private ArgumentCaptor<List<ItemViewConfigAndItemSkuRequest>> itemViewConfigAndItemSkuRequestListCaptor;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequestCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionCaptor;

  @InjectMocks
  private NeedCorrectionServiceBean needCorrectionService;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductMailEventService productMailEventService;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);

    productCollection.setProductId(PRODUCT_ID);
    productCollection.setId(PRODUCT_ID);

    descriptiveProductAttributeDomainEventModel.setAttribute(new AttributeDomainEventModel());
    descriptiveProductAttributeDomainEventModel.getAttribute()
        .setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    descriptiveProductAttributeValueDomainEventModel.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_VALUE);
    descriptiveProductAttributeDomainEventModel
        .setProductAttributeValues(Arrays.asList(descriptiveProductAttributeValueDomainEventModel));
    descriptiveProductAttributeDomainEventModel.getAttribute().setAttributeCode(ATTRIBUTE_CODE);
    definingProductAttributeDomainEventModel.setAttribute(new AttributeDomainEventModel());
    definingProductAttributeDomainEventModel.getAttribute().setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    definingProductAttributeDomainEventModel.getAttribute().setAttributeCode(DEFINING_ATTRIBUTE_CODE);
    definingProductAttributeValueDomainEventModel.setAllowedAttributeValue(new AllowedAttributeValueDomainEventModel(
        StringUtils.EMPTY, PRODUCT_ATTRIBUTE_VALUE, 0));
    definingProductAttributeDomainEventModel
        .setProductAttributeValues(Arrays.asList(definingProductAttributeValueDomainEventModel));
    predefinedProductAttributeDomainEventModel.setAttribute(new AttributeDomainEventModel());
    predefinedProductAttributeDomainEventModel.getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedProductAttributeDomainEventModel.getAttribute()
      .setAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    predefinedProductAttributeValueDomainEventModel
        .setPredefinedAllowedAttributeValue(
            new PredefinedAllowedAttributeValueDomainEventModel(StringUtils.EMPTY, PRODUCT_ATTRIBUTE_VALUE, 0));
    predefinedProductAttributeDomainEventModel
        .setProductAttributeValues(Arrays.asList(predefinedProductAttributeValueDomainEventModel));
    productItemAttributeValueDomainEventModel.setAttribute(new AttributeDomainEventModel());
    productItemAttributeValueDomainEventModel.getAttribute().setAttributeCode(ATTRIBUTE_CODE);
    productItemAttributeValueDomainEventModel.setValue(ITEM_ATTRIBUTE_VALUE);

    productItemAttributeValueDomainEventModel1.setAttribute(new AttributeDomainEventModel());
    productItemAttributeValueDomainEventModel1.getAttribute()
      .setAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    productItemAttributeValueDomainEventModel1.setValue(ITEM_ATTRIBUTE_VALUE);
    productItemAttributeValueDomainEventModel1.getAttribute()
      .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    imageDomainEventModel.setLocationPath(LOCATION_PATH);
    pdtProductItemDomainEventModel.setImages(Arrays.asList(imageDomainEventModel));
    pdtItemNotesDomainEventModel.setVendorErrorFields(Arrays.asList(VENDOR_NOTES_FIELD));
    pdtItemNotesDomainEventModel.setSkuCode(SKU_CODE);
    pdtProductItemDomainEventModel.setItemNotes(pdtItemNotesDomainEventModel);
    pdtProductItemDomainEventModel.setSkuCode(SKU_CODE);
    pdtProductItemDomainEventModel.setProductItemAttributeValues(
      Arrays.asList(productItemAttributeValueDomainEventModel,
        productItemAttributeValueDomainEventModel1));
    BeanUtils.copyProperties(pdtProductItemDomainEventModel, pdtProductItemDomainEventModel1);
    pdtProductItemDomainEventModel1.setSkuCode(SKU_CODE_1);
    categoryDomainEventModel.setCategoryCode(CATEGORY_CODE);
    categoryDomainEventModel.setName(CATEGORY_NAME);
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    pdtProductDomainEventModel.setProductCategories(Arrays.asList(productCategoryDomainEventModel));
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    pdtProductDomainEventModel.setBrand(BRAND_NAME);
    pdtProductDomainEventModel.setBrandCode(BRAND_CODE);
    pdtProductNotesDomainEventModel.setAllVariants(true);
    pdtProductNotesDomainEventModel.setContentAdditionalNotes(VENDOR_NOTES_FIELD);
    pdtProductNotesDomainEventModel.setImageReason(Arrays.asList(VENDOR_NOTES_FIELD));
    pdtProductNotesDomainEventModel.setVendorErrorFields(Arrays.asList(VENDOR_NOTES_FIELD));
    pdtProductDomainEventModel.setProductNotes(pdtProductNotesDomainEventModel);
    pdtProductDomainEventModel.setImages(Arrays.asList(imageDomainEventModel));
    pdtProductDomainEventModel.setPostLive(true);
    pdtProductDomainEventModel.setProductItems(
      Arrays.asList(pdtProductItemDomainEventModel, pdtProductItemDomainEventModel1));
    pdtProductDomainEventModel.setProductAttributes(Arrays.asList(descriptiveProductAttributeDomainEventModel,
        definingProductAttributeDomainEventModel, predefinedProductAttributeDomainEventModel));

    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(ITEM_ID);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU_1);
    productItemBusinessPartner1.setProductItemId(ITEM_ID_1);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    productItemDetailResponse.setId(ITEM_ID);
    productItemDetailResponse.setSkuCode(SKU_CODE);
    productItemAttributeValueResponse.setValue(ITEM_ATTRIBUTE_VALUE);
    productItemAttributeValueResponse.setAttributeResponse(new AttributeResponse());
    productItemAttributeValueResponse.getAttributeResponse()
      .setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productItemAttributeValueResponse.getAttributeResponse()
      .setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productItemAttributeValueResponse.getAttributeResponse().setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));
    productItemAttributeValueResponse.getAttributeResponse().setAttributeCode(ATTRIBUTE_CODE);
    productItemDetailResponse.setProductItemAttributeValueResponses(
      Arrays.asList(productItemAttributeValueResponse));
    productItemDetailResponse1.setId(ITEM_ID_1);
    productItemDetailResponse1.setSkuCode(SKU_CODE_1);
    productItemDetailResponse1.setProductItemAttributeValueResponses(
      Arrays.asList(productItemAttributeValueResponse));
    productCategoryResponse.setCategory(new CategoryResponse());
    productCategoryResponse.getCategory().setCatalog(new CatalogResponse());
    productCategoryResponse.getCategory().setCategoryCode(CATEGORY_CODE);
    productCategoryResponse.getCategory().setName(CATEGORY_NAME);
    oldProductCategoryResponse.setCategory(new CategoryResponse());
    oldProductCategoryResponse.getCategory().setCatalog(new CatalogResponse());
    oldProductCategoryResponse.getCategory().setCategoryCode(OLD_CATEGORY_CODE);
    oldProductCategoryResponse.getCategory().setName(OLD_CATEGORY_NAME);
    descriptiveAttributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    descriptiveAttributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    descriptiveProductAttributeValueResponse.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_VALUE);
    descriptiveProductAttributeResponse.setAttribute(descriptiveAttributeResponse);
    descriptiveProductAttributeResponse.setProductAttributeValues(Arrays.asList(
      descriptiveProductAttributeValueResponse));

    predefinedAttributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttributeResponse.setAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    predefinedProductAttributeValueResponse.setPredefinedAllowedAttributeValue(
      new PredefinedAllowedAttributeValueResponse());
    predefinedProductAttributeValueResponse.getPredefinedAllowedAttributeValue()
      .setValue(PRODUCT_ATTRIBUTE_VALUE);
    predefinedProductAttributeResponse.setAttribute(predefinedAttributeResponse);
    predefinedProductAttributeResponse.setProductAttributeValues(Arrays.asList(
      predefinedProductAttributeValueResponse));

    definingAttributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    definingAttributeResponse.setAttributeCode(DEFINING_ATTRIBUTE_CODE);
    definingProductAttributeValueResponse.setAllowedAttributeValue(
      new AllowedAttributeValueResponse());
    definingProductAttributeValueResponse.getAllowedAttributeValue()
      .setValue(DEFINING_ATTRIBUTE_CODE);
    definingProductAttributeValueResponse.getAllowedAttributeValue()
      .setValue(PRODUCT_ATTRIBUTE_VALUE);
    definingProductAttributeResponse.setAttribute(definingAttributeResponse);
    definingProductAttributeResponse.setProductAttributeValues(Arrays.asList(
      definingProductAttributeValueResponse));
    productDetailResponse.setProductAttributeResponses(
      Arrays.asList(descriptiveProductAttributeResponse, predefinedProductAttributeResponse,
        definingProductAttributeResponse));
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    productDetailResponse.setProductItemResponses(new HashSet<>(Arrays.asList(productItemDetailResponse,
        productItemDetailResponse1)));
    when(this.productOutbound.getProductDetailByProductCode(PRODUCT_CODE, false, false))
        .thenReturn(productDetailResponse);
    when(this.objectMapper.writeValueAsString(Mockito.any(NeedRevisionNotes.class))).thenReturn(StringUtils.EMPTY);

    predefinedAllowedAttributeValueResponse.setValue(PRODUCT_ATTRIBUTE_VALUE);
    when(this.predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(
      Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE))).thenReturn(
      predefinedAllowedAttributeValueResponse);
    when(productOutbound.getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE)).thenReturn(
      predefinedAttributeResponse);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    categoryDetailResponse.setName(CATEGORY_NAME);
    categoryDetailResponse.setCatalog(new CatalogResponse());
    newProductAttributeDomainEventModel.setAttribute(new AttributeDomainEventModel());
    newProductAttributeDomainEventModel.getAttribute()
      .setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    newProductAttributeValueDomainEventModel.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_VALUE);
    newProductAttributeDomainEventModel
      .setProductAttributeValues(Arrays.asList(newProductAttributeValueDomainEventModel));
    newProductAttributeDomainEventModel.getAttribute().setAttributeCode(NEW_ATTRIBUTE_CODE);

    productBusinessPartnerCounter = new ProductBusinessPartnerCounter();
    productBusinessPartnerCounter.setAppealedProductCount(10);
    productBusinessPartnerCounter.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productDistributionTaskRepository);
    verifyNoMoreInteractions(productLevel1CollectionService);
    verifyNoMoreInteractions(productBusinessPartnerService);
    verifyNoMoreInteractions(productOutbound);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(solrActiveProductCollectionService);
    verifyNoMoreInteractions(predefinedAttributeAllowedValueService);
    verifyNoMoreInteractions(productLevel3Service);
    verifyNoMoreInteractions(productLevel1HistoryService);
    verifyNoMoreInteractions(solrReviewProductCollectionService);
    verifyNoMoreInteractions(productLevel3WipService);
    verifyNoMoreInteractions(productLevel1WipService);
    verifyNoMoreInteractions(productMailEventService);
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(productAppealService);
    verifyNoMoreInteractions(productBusinessPartnerRepository);
  }

  @Test
  public void sendForNeedCorrectionByProductCodeTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setProductType(1);
    productBusinessPartner.setAppealedProduct(true);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setStoreId(STORE_ID);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(productBusinessPartnerRepository.findFirstByGdnProductSku(PRODUCT_CODE))
        .thenReturn(productBusinessPartner);
    when(productAppealService.fetchThresholdAndCounterForAppealProduct(eq(STORE_ID), any()))
        .thenReturn(Pair.of(10, productBusinessPartnerCounter));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Map<String, String> map = new HashMap<>();
    map.put(SKU_CODE_1, ITEM_ID_1);
    map.put(SKU_CODE, ITEM_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, map, productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService)
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE),
            eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    verify(productAppealService).fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(productAppealService).decrementCounterForProductAppeal(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(productBusinessPartnerService).saveProductBusinessPartner(any());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME, productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCodeNotAppealedTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setProductType(1);
    productBusinessPartner.setAppealedProduct(false);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setStoreId(STORE_ID);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(productBusinessPartnerRepository.findFirstByGdnProductSku(PRODUCT_CODE))
        .thenReturn(productBusinessPartner);
    when(productAppealService.fetchThresholdAndCounterForAppealProduct(eq(STORE_ID), any()))
        .thenReturn(Pair.of(10, productBusinessPartnerCounter));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Map<String, String> map = new HashMap<>();
    map.put(SKU_CODE_1, ITEM_ID_1);
    map.put(SKU_CODE, ITEM_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, map, productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());
    verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());
    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService)
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE),
            eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME, productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCodeFailedToPreDefinedAttributeValueTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setProductType(1);
    when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString())).thenThrow(Exception.class);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Map<String, String> map = new HashMap<>();
    map.put(SKU_CODE_1, ITEM_ID_1);
    map.put(SKU_CODE, ITEM_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, map, productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        eq(PREDEFINED_ATTRIBUTE_CODE), eq(Constants.HYPHEN));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME, productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCodeFailedToPreDefinedAttributeValueHyphenTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setProductType(1);
    when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Constants.DEFAULT_REQUEST_ID,
        PREDEFINED_ATTRIBUTE_CODE, PRODUCT_ATTRIBUTE_VALUE)).thenThrow(Exception.class);
    when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Constants.DEFAULT_REQUEST_ID,
        PREDEFINED_ATTRIBUTE_CODE, Constants.HYPHEN)).thenReturn(predefinedAllowedAttributeValueResponse);
    when(this.productDistributionTaskRepository.getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE)).thenReturn(pdtProductDomainEventModel);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID)).thenReturn(
        Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository).getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Map<String, String> map = new HashMap<>();
    map.put(SKU_CODE_1, ITEM_ID_1);
    map.put(SKU_CODE, ITEM_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, map, productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        eq(PREDEFINED_ATTRIBUTE_CODE), eq(Constants.HYPHEN));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService).createForNeedRevision(eq(PRODUCT_CODE),
        eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(), any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME, productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCodeEmptyAttributeValueHyphenTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setProductType(1);
    pdtProductDomainEventModel.getProductAttributes().stream().filter(
            productAttributeDomainEventModel -> AttributeType.PREDEFINED_ATTRIBUTE.name()
                .equals(productAttributeDomainEventModel.getAttribute().getAttributeType())).findFirst().get()
        .getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setValue(StringUtils.EMPTY);
    when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Constants.DEFAULT_REQUEST_ID,
        PREDEFINED_ATTRIBUTE_CODE, PRODUCT_ATTRIBUTE_VALUE)).thenThrow(Exception.class);
    when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Constants.DEFAULT_REQUEST_ID,
        PREDEFINED_ATTRIBUTE_CODE, Constants.HYPHEN)).thenReturn(predefinedAllowedAttributeValueResponse);
    when(this.productDistributionTaskRepository.getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE)).thenReturn(pdtProductDomainEventModel);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID)).thenReturn(
        Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository).getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Map<String, String> map = new HashMap<>();
    map.put(SKU_CODE_1, ITEM_ID_1);
    map.put(SKU_CODE, ITEM_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, map, productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService).createForNeedRevision(eq(PRODUCT_CODE),
        eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(), any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME, productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }


  @Test
  public void sendForNeedCorrectionByProductCodeUpdatePDTFalseTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setProductType(1);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, false);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Map<String, String> map = new HashMap<>();
    map.put(SKU_CODE_1, ITEM_ID_1);
    map.put(SKU_CODE, ITEM_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, map, productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
  }

  @Test
  public void sendForNeedCorrectionByProductCodeAutoNeedRevisionTest() throws Exception {
    NeedRevisionNotes needRevisionNotes = new NeedRevisionNotes();
    needRevisionNotes.setAllVariants(true);
    needRevisionNotes.setImagesAdditionalNotes(AUTO_NEED_REVISION_NOTES);
    needRevisionNotes.setImageReason(Collections.singletonList(Constants.OTHERS_NOTES_REASON));
    AutoNeedRevisionRequest autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    autoNeedRevisionRequest.setImageReason(AUTO_NEED_REVISION_NOTES);
    ObjectMapper mapper = new ObjectMapper();
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Collections.singletonList(productBusinessPartner));
    when(this.objectMapper.writeValueAsString(Mockito.any(NeedRevisionNotes.class)))
        .thenReturn(mapper.writeValueAsString(needRevisionNotes));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, true, autoNeedRevisionRequest, true);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
        Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.SYSTEM+Constants.HYPHEN+Constants.AUTO_NEED_REVISION));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertTrue(productCollectionCaptor.getValue().isAutoNeedRevision());
    assertEquals(1, productCollectionCaptor.getValue().getAutoNeedRevisionCount());

    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
    assertEquals(mapper.writeValueAsString(needRevisionNotes),
        productCollectionCaptor.getValue().getNeedCorrectionNotes());
  }

  @Test
  public void sendForNeedCorrectionByProductCodePreliveTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setPostLive(false);
    pdtProductItemDomainEventModel.setProductItemAttributeValues(null);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
        Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCodePreliveAndEditedTrueTest() throws Exception {
    pdtProductNotesDomainEventModel.setCommonImageReason(Arrays.asList("Image reason"));
    pdtProductDomainEventModel.setProductNotes(pdtProductNotesDomainEventModel);
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setPostLive(false);
    pdtProductDomainEventModel.setEdited(true);
    pdtProductItemDomainEventModel.setProductItemAttributeValues(null);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
        Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCode_updatedByFromPayloadTest() throws Exception {
    pdtProductDomainEventModel.getProductNotes().setImageReason(null);
    pdtProductDomainEventModel.getProductItems().get(0).getItemNotes()
        .setVendorNotes(Arrays.asList(VENDOR_NOTES_FIELD));
    pdtProductDomainEventModel.setUpdatedBy(Constants.DEFAULT_USERNAME);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
      Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
        productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
        productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
        productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
  }

  @Test
  public void sendForNeedCorrectionByProductCode_withCategoryChangeTest() throws Exception {
    pdtProductDomainEventModel.getProductNotes().setVendorNotes(Arrays.asList(VENDOR_NOTES_FIELD));
    pdtProductDomainEventModel.getProductNotes().setImageReason(null);
    pdtProductDomainEventModel.getProductNotes().setImagesAdditionalNotes(VENDOR_NOTES_FIELD);
    productCategoryResponse.setMarkForDelete(true);
    productDetailResponse.setProductCategoryResponses(
      Arrays.asList(productCategoryResponse, oldProductCategoryResponse));
    pdtProductDomainEventModel.setProductAttributes(
      Arrays.asList(descriptiveProductAttributeDomainEventModel,
        predefinedProductAttributeDomainEventModel, definingProductAttributeDomainEventModel,
        newProductAttributeDomainEventModel));
    when(productOutbound.getAttributeDetailByAttributeCode(NEW_ATTRIBUTE_CODE)).thenReturn(
      descriptiveAttributeResponse);
    when(this.productOutbound
      .getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    when(this.productDistributionTaskRepository
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
      .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
      .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.productDistributionTaskRepository)
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
      Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(this.productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    verify(productOutbound).getAttributeDetailByAttributeCode(NEW_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
      productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
      productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCode_withCategoryChangeTest_SKuTrue() throws Exception {
    predefinedAttributeResponse.setSkuValue(true);
    when(productOutbound.getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE)).thenReturn(
        predefinedAttributeResponse);
    pdtProductDomainEventModel.getProductNotes().setVendorNotes(Arrays.asList(VENDOR_NOTES_FIELD));
    pdtProductDomainEventModel.getProductNotes().setImageReason(null);
    pdtProductDomainEventModel.getProductNotes().setImagesAdditionalNotes(VENDOR_NOTES_FIELD);
    productCategoryResponse.setMarkForDelete(true);
    productDetailResponse.setProductCategoryResponses(
        Arrays.asList(productCategoryResponse, oldProductCategoryResponse));
    pdtProductDomainEventModel.setProductAttributes(
        Arrays.asList(descriptiveProductAttributeDomainEventModel,
            predefinedProductAttributeDomainEventModel, definingProductAttributeDomainEventModel,
            newProductAttributeDomainEventModel));
    descriptiveAttributeResponse.setSkuValue(true);
    when(productOutbound.getAttributeDetailByAttributeCode(NEW_ATTRIBUTE_CODE)).thenReturn(
        descriptiveAttributeResponse);
    when(this.productOutbound
        .getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    when(this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
        .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
        productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(this.productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    verify(productOutbound).getAttributeDetailByAttributeCode(NEW_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
  }

  @Test
  public void sendForNeedCorrectionByProductCode_nullProductNotesEmptyPBPL3Test() throws Exception {
    pdtProductDomainEventModel.setProductNotes(null);
    when(this.productDistributionTaskRepository
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
      .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
      .thenReturn(Collections.emptyList());
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
      Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
      productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
      productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrectionByProductCode_nullItemNotesEmptyTest() throws Exception {
    pdtProductDomainEventModel.getProductItems().get(0).setItemNotes(null);
    when(this.productDistributionTaskRepository
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
      .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
      .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    verify(this.productDistributionTaskRepository)
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel3Service).takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(),
        productDetailResponse);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
      Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound, times(2)).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(productLevel1HistoryService)
        .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
            any(), eq(Constants.DEFAULT_USERNAME));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
      productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
      productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void sendForNeedCorrection_NullImageReasonTest() throws Exception {
    AutoNeedRevisionRequest autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    autoNeedRevisionRequest.setImageReason(null);
    pdtProductDomainEventModel.getProductItems().get(1).setItemNotes(null);
    pdtProductDomainEventModel.setPostLive(false);
    pdtProductItemDomainEventModel.setProductItemAttributeValues(null);
    when(this.productDistributionTaskRepository
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE))
      .thenReturn(pdtProductDomainEventModel);
    when(this.productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(
      productCollection);
    when(this.productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
      .thenReturn(Arrays.asList(productBusinessPartner));
    needCorrectionService.sendForNeedCorrectionByProductCode(PRODUCT_CODE, true,
      autoNeedRevisionRequest, true);
    verify(this.productDistributionTaskRepository)
      .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, PRODUCT_CODE);
    verify(this.productLevel1CollectionService).findByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(this.productLevel1CollectionService).updateAndSendForCorrection(productCollectionCaptor.capture());
    verify(this.productOutbound).updateAndMarkProductForNeedCorrection(productRequestCaptor.capture());
    verify(this.productOutbound).getProductDetailByProductCode(PRODUCT_CODE, false, false);
    verify(this.objectMapper).writeValueAsString(Mockito.any(NeedRevisionNotes.class));
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    verify(productBusinessPartnerService).update(Mockito.any(ProductBusinessPartner.class));
    verify(this.predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(
      Mockito.any(), eq(PREDEFINED_ATTRIBUTE_CODE), eq(PRODUCT_ATTRIBUTE_VALUE));
    verify(productOutbound).getAttributeDetailByAttributeCode(PREDEFINED_ATTRIBUTE_CODE);
    verify(productLevel1HistoryService)
      .createForNeedRevision(eq(PRODUCT_CODE), eq(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue()), any(),
        any(), eq(Constants.SYSTEM+Constants.HYPHEN+Constants.AUTO_NEED_REVISION));
    verify(solrReviewProductCollectionService).deleteProductFromReviewProductCollection(productCollection.getId());
    assertEquals(NEED_CORRECTION_STATE, productCollectionCaptor.getValue().getState());
    assertEquals(BRAND_NAME, productCollectionCaptor.getValue().getBrand());
    assertEquals(BRAND_CODE, productCollectionCaptor.getValue().getBrandCode());
    assertFalse(productCollectionCaptor.getValue().isActivated());
    assertFalse(productCollectionCaptor.getValue().isViewable());
    assertEquals(CATEGORY_CODE, productCollectionCaptor.getValue().getCategoryCode());
    assertEquals(BRAND_CODE, productRequestCaptor.getValue().getBrandCode());
    assertEquals(BRAND_NAME, productRequestCaptor.getValue().getBrand());
    assertNotNull(productRequestCaptor.getValue().getProductCategories().get(0));
    assertEquals(CATEGORY_NAME,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getName());
    assertEquals(CATEGORY_CODE,
      productRequestCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(PRODUCT_ATTRIBUTE_VALUE,
      productRequestCaptor.getValue().getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue());
    assertEquals(LOCATION_PATH, productRequestCaptor.getValue().getImages().get(0).getLocationPath());
    assertEquals(LOCATION_PATH,
      productRequestCaptor.getValue().getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void updateStateInPBPAndSendProductToNeedRevisionTest() throws Exception {
    Mockito.when(productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Collections.singletonList(productBusinessPartner));
    needCorrectionService
        .updateStateInPBPAndSendProductToNeedRevision(PRODUCT_CODE, AUTO_NEED_REVISION_NOTES, new NeedRevisionNotes(),
            true, true, false, productCollection);
    Mockito.verify(productLevel1WipService)
        .returnDraftForCorrection(PRODUCT_CODE, new NeedRevisionNotes(), true, true, false);
    Mockito.verify(productLevel3WipService).returnDraftForCorrection(PRODUCT_CODE, AUTO_NEED_REVISION_NOTES);
    Mockito.verify(productLevel1HistoryService)
        .createForNeedRevision(PRODUCT_CODE, WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue(),
            AUTO_NEED_REVISION_NOTES, new NeedRevisionNotes(),
            Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_NEED_REVISION);
    Mockito.verify(productMailEventService).sendDomainEventForSentForCorrection(PRODUCT_CODE, AUTO_NEED_REVISION_NOTES);
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(PRODUCT_CODE, ProductStatus.NEED_CORRECTION, AUTO_NEED_REVISION_NOTES);
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    Mockito.verify(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(productCollection.getProductId());
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productLevel1CollectionService).updateProductWorkFlowStateForNeedRevision(PRODUCT_CODE);
  }
  @Test
  public void updateStateInPBPAndSendProductToNeedRevisionAutoNeedRevisionFalseTest() throws Exception {
    Mockito.when(productLevel1CollectionService.findByProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(Collections.singletonList(productBusinessPartner));
    needCorrectionService
        .updateStateInPBPAndSendProductToNeedRevision(PRODUCT_CODE, AUTO_NEED_REVISION_NOTES, new NeedRevisionNotes(),
            false, true, false, productCollection);
    Mockito.verify(productLevel1WipService)
        .returnDraftForCorrection(PRODUCT_CODE, new NeedRevisionNotes(), false, true, false);
    Mockito.verify(productLevel3WipService).returnDraftForCorrection(PRODUCT_CODE, AUTO_NEED_REVISION_NOTES);
    Mockito.verify(productLevel1HistoryService)
        .createForNeedRevision(PRODUCT_CODE, WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue(),
            AUTO_NEED_REVISION_NOTES, new NeedRevisionNotes(), StringUtils.EMPTY);
    Mockito.verify(productMailEventService).sendDomainEventForSentForCorrection(PRODUCT_CODE, AUTO_NEED_REVISION_NOTES);
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(PRODUCT_CODE, ProductStatus.NEED_CORRECTION, AUTO_NEED_REVISION_NOTES);
        verify(solrActiveProductCollectionService).deleteSolrProductCollectionDocument(productCollection.getId());

    Mockito.verify(solrReviewProductCollectionService)
        .deleteProductFromReviewProductCollection(productCollection.getProductId());
    Mockito.verify(productBusinessPartnerService).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productLevel1CollectionService).updateProductWorkFlowStateForNeedRevision(PRODUCT_CODE);
  }

}