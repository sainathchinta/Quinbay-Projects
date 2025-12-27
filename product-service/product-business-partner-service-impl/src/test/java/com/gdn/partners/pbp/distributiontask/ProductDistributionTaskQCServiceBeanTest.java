package com.gdn.partners.pbp.distributiontask;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.service.ProductAppealService;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
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
import com.gdn.mta.product.util.BeanUtils;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ApproveProductService;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductDistributionService;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.PredefinedAttributeAllowedValueService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTImageDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductItemDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
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
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ProductDistributionTaskQCServiceBeanTest {

  private static final String DEFAULT_ATTRIBUTE_CODE_1 = "ATT-00001";
  private static final String DEFAULT_ATTRIBUTE_CODE_2 = "ATT-00002";
  private static final String DEFAULT_ATTRIBUTE_CODE_3 = "ATT-00003";
  private static final String DEFAULT_ATTRIBUTE_CODE_4 = "ATT-00004";
  private static final String DEFAULT_ATTRIBUTE_CODE_5 = "ATT-00005";
  private static final String DEFAULT_SKU_CODE = "SKU-00001";
  private static final String DEFAULT_SKU_CODE_2 = "SKU-00002";
  private static final String DEFAULT_VALUE = "VALUE";
  private static final String PRODUCT_CODE = "PROD_CODE";
  private static final String STORE_ID = "10001";
  private static final Long TIMESTAMP = System.currentTimeMillis();
  private static final Long VERSION = 1L;
  private static final String PRODUCT_ID = "productId";
  private static final String ITEM_ATTRIBUTE_VALUE_1 = "value1";
  private static final String ITEM_ATTRIBUTE_VALUE_2 = "value2";
  private static final String ITEM_ATTRIBUTE_VALUE_3 = "value3";
  private static final Integer DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  private static final Integer DANGEROUS_GOODS_LEVEL_2 = 2;
  private static final String BRAND_APPROVAL_STATUS = "approved";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND = "BRAND";

  private static final String BUSINESS_PARTNER_CODE = "MTA-1234";
  private static final String PRODUCT_IMAGE_HASH_1 = "1234";
  private static final String PRODUCT_IMAGE_LOCATION = "product-image";
  private static final String PRODUCT_ITEM_IMAGE_LOCATION = "item-image";
  private static final String ITEM_IMAGE_HASH_1 = "123456";
  private static final String SYSTEM_REVIEW = "System-Review";
  private static final String CATEGORY_CODE = "category_code";

  @Mock
  private ProductService productService;

  @Mock
  private PredefinedAttributeAllowedValueService predefinedAttributeAllowedValueService;

  @InjectMocks
  private ProductDistributionTaskQCServiceBean productDistributionTaskQCServiceBean;

  private ProductRequest productRequest;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ApproveProductService approveProductService;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductDistributionService productDistributionService;

  @Mock
  private ProductAppealService productAppealService;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<AutoApprovalTypeRequest> autoApprovalTypeRequestArgumentCaptor;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  private ProfileResponse profileResponse;

  private ProductResponse productBasicDetailsResponse;
  private ProductDetailResponse productDetailResponse;
  private ProductCollection productCollection;
  private String RESIZE = "resize";
  private ProductBusinessPartnerCounter productBusinessPartnerCounter;

  private ProductResponse generateProductResponse(){
    ProductResponse productResponse = new ProductResponse();
    productResponse.setViewable(true);
    productResponse.setActivated(true);
    productResponse.setProductCode(PRODUCT_CODE);
    return productResponse;
  }

  private ProductResponse generateProductBasicDetailsResponse(){
    ProductResponse productResponse = new ProductResponse();
    productResponse.setViewable(true);
    productResponse.setActivated(true);
    productResponse.setProductCode(PRODUCT_CODE);
    productResponse.setVersion(VERSION);
    productResponse.setId(PRODUCT_ID);
    return productResponse;
  }

  private ProductDetailResponse generateProductData() {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<AllowedAttributeValueResponse>();
    allowedAttributeValueResponses.add(new AllowedAttributeValueResponse());

    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeResponses =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    predefinedAllowedAttributeResponses.add(new PredefinedAllowedAttributeValueResponse());

    AttributeResponse attributeData1 = new AttributeResponse();
    attributeData1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_1);
    attributeData1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    attributeData1.setAllowedAttributeValues(allowedAttributeValueResponses);
    attributeData1.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeResponses);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse.setAttributeResponse(attributeData1);
    List<ProductItemAttributeValueResponse> productItemAttributeValueResponses =
        new ArrayList<ProductItemAttributeValueResponse>();
    productItemAttributeValueResponses.add(productItemAttributeValueResponse);

    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(DEFAULT_SKU_CODE);
    productItemResponse.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    productItemResponse.setProductItemAttributeValueResponses(productItemAttributeValueResponses);
    Set<ProductItemResponse> productItemResponses = new HashSet<ProductItemResponse>();
    productItemResponses.add(productItemResponse);

    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCatalog(new CatalogResponse());
    categoryResponse.setCategoryCode(DEFAULT_SKU_CODE);
    categoryResponse.setName(DEFAULT_SKU_CODE);
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(categoryResponse);
    List<ProductCategoryResponse> productCategoryResponses = new ArrayList<ProductCategoryResponse>();
    productCategoryResponses.add(productCategoryResponse);

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setProductCode(PRODUCT_CODE);
    productData.setStoreId(STORE_ID);
    productData.setProductCategoryResponses(productCategoryResponses);
    productData.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    CategoryResponse categoryData = new CategoryResponse();
    ProductCategoryResponse productCategoryData = new ProductCategoryResponse();
    productCategoryData.setCategory(categoryData);
    productData.getProductCategoryResponses().add(productCategoryData);
    productData.setProductItemResponses(productItemResponses);
    productData.setVersion(1L);
    productData.setId(PRODUCT_ID);
    Image productImage = new Image();
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(false);
    itemImage2.setSequence(2);
    productData.setImages(Arrays.asList(productImage,productImage2));
    ProductAttributeResponse productAttributeData1 = new ProductAttributeResponse();
    productAttributeData1.setAttribute(attributeData1);
    productAttributeData1.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    ProductAttributeValueResponse productAttributeValueData1 = new ProductAttributeValueResponse();
    productAttributeValueData1.setAllowedAttributeValue(new AllowedAttributeValueResponse());
    productAttributeValueData1.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueResponse());
    productAttributeData1.getProductAttributeValues().add(productAttributeValueData1);

    AttributeResponse attributeData2 = new AttributeResponse();
    attributeData2.setSkuValue(true);
    attributeData2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeData2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    ProductAttributeResponse productAttributeData2 = new ProductAttributeResponse();
    productAttributeData2.setAttribute(attributeData2);
    productAttributeData2.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    ProductAttributeValueResponse productAttributeValueData2 = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueData =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueData.setValue(DEFAULT_ATTRIBUTE_CODE_2);
    productAttributeValueData2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueData);
    productAttributeData2.getProductAttributeValues().add(productAttributeValueData2);

    AttributeResponse attributeData3 = new AttributeResponse();
    attributeData3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeData3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductAttributeResponse productAttributeData3 = new ProductAttributeResponse();
    productAttributeData3.setAttribute(attributeData3);
    productAttributeData3.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    ProductAttributeValueResponse productAttributeValueData3 = new ProductAttributeValueResponse();
    productAttributeData3.getProductAttributeValues().add(productAttributeValueData3);
    productData.getProductAttributeResponses().add(productAttributeData1);
    productData.getProductAttributeResponses().add(productAttributeData2);
    productData.getProductAttributeResponses().add(productAttributeData3);
    productData.getProductItemResponses().iterator().next()
      .setImages(Arrays.asList(itemImage, itemImage2));
    return productData;
  }

  private PDTProductDomainEventModel generatePdtProductDomainEventModel() {
    productRequest = generateProductRequestForProcessImage();
    PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setPostLive(false);
    pdtProductDomainEventModel.setProductItems(new ArrayList<PDTProductItemDomainEventModel>());
    pdtProductDomainEventModel.setProductAttributes(new ArrayList<ProductAttributeDomainEventModel>());
    pdtProductDomainEventModel.setImages(new ArrayList<ImageDomainEventModel>());
    ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
    imageDomainEventModel.setLocationPath(PRODUCT_IMAGE_LOCATION);
    imageDomainEventModel.setCommonImage(true);
    imageDomainEventModel.setSequence(0);
    imageDomainEventModel.setMainImage(true);
    PDTImageDomainEventModel pdtImageDomainEventModel = new PDTImageDomainEventModel();

    PDTProductItemDomainEventModel pdtProductItemDomainEventModel = new PDTProductItemDomainEventModel();
    pdtProductItemDomainEventModel.setImages(new ArrayList<ImageDomainEventModel>());
    ImageDomainEventModel imageItemDomainEventModel = new ImageDomainEventModel();
    imageItemDomainEventModel.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    imageItemDomainEventModel.setCommonImage(true);
    imageItemDomainEventModel.setSequence(0);
    imageItemDomainEventModel.setMainImage(true);
    pdtProductItemDomainEventModel.getImages().add(imageItemDomainEventModel);
    pdtProductItemDomainEventModel.getPdtImageDomainEventModels().add(pdtImageDomainEventModel);
    pdtProductItemDomainEventModel.setSkuCode(DEFAULT_SKU_CODE);
    pdtProductItemDomainEventModel.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL_2);
    pdtProductDomainEventModel.getProductItems().add(pdtProductItemDomainEventModel);
    pdtProductDomainEventModel.getProductItems().add(pdtProductItemDomainEventModel);
    pdtProductDomainEventModel.getImages().add(imageItemDomainEventModel);
    pdtProductDomainEventModel.getPdtImageDomainEventModels().add(pdtImageDomainEventModel);
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);

    AttributeDomainEventModel attributeDomainEventModel1 = new AttributeDomainEventModel();
    attributeDomainEventModel1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_1);
    attributeDomainEventModel1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    ProductAttributeDomainEventModel productAttributeDomainEventModel1 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel1.setAttribute(attributeDomainEventModel1);
    productAttributeDomainEventModel1.setProductAttributeValues(new ArrayList<ProductAttributeValueDomainEventModel>());
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel1 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel1.setAllowedAttributeValue(new AllowedAttributeValueDomainEventModel());
    productAttributeValueDomainEventModel1
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueDomainEventModel());

    productAttributeDomainEventModel1.getProductAttributeValues().add(productAttributeValueDomainEventModel1);
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(DEFAULT_SKU_CODE);
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    pdtProductDomainEventModel.setProductCategories(Arrays.asList(productCategoryDomainEventModel));
    AttributeDomainEventModel attributeDomainEventModel2 = new AttributeDomainEventModel();
    attributeDomainEventModel2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeDomainEventModel2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    ProductAttributeDomainEventModel productAttributeDomainEventModel2 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel2.setAttribute(attributeDomainEventModel2);
    productAttributeDomainEventModel2.setProductAttributeValues(new ArrayList<ProductAttributeValueDomainEventModel>());
    PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValueDomainEventModel =
        new PredefinedAllowedAttributeValueDomainEventModel();
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel2 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel2
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
    productAttributeDomainEventModel2.getProductAttributeValues().add(productAttributeValueDomainEventModel2);

    AttributeDomainEventModel attributeDomainEventModel3 = new AttributeDomainEventModel();
    attributeDomainEventModel3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeDomainEventModel3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());

    ProductAttributeDomainEventModel productAttributeDomainEventModel3 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel3.setAttribute(attributeDomainEventModel3);
    productAttributeDomainEventModel3.setProductAttributeValues(new ArrayList<ProductAttributeValueDomainEventModel>());
    predefinedAllowedAttributeValueDomainEventModel =
        new PredefinedAllowedAttributeValueDomainEventModel();
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel3 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel3
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
    productAttributeDomainEventModel3.getProductAttributeValues().add(productAttributeValueDomainEventModel3);

    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel1);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel2);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel3);
    return pdtProductDomainEventModel;
  }

  private boolean isImageValidForScaling(Image image) {
    return !image.isMarkForDelete() &&
      (Objects.isNull(image.getOriginalImage()) || Boolean.FALSE.equals(image.getOriginalImage())) &&
      (image.isActive() || !image.getLocationPath().contains(RESIZE));
  }

  private PDTProductDomainEventModel generatePdtProductDomainEventModelForCategoryChange() {
    productRequest = generateProductRequestForProcessImage();
    PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setPostLive(false);
    pdtProductDomainEventModel.setProductItems(new ArrayList<>());
    pdtProductDomainEventModel.setProductAttributes(new ArrayList<>());
    ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel(true,
      PRODUCT_IMAGE_LOCATION,0);
    pdtProductDomainEventModel.setImages(new ArrayList<>());
    PDTProductItemDomainEventModel pdtProductItemDomainEventModel = new PDTProductItemDomainEventModel();
    pdtProductItemDomainEventModel.setImages(new ArrayList<>());
    pdtProductItemDomainEventModel.getImages().add(imageDomainEventModel);
    pdtProductItemDomainEventModel.setSkuCode(DEFAULT_SKU_CODE);
    pdtProductDomainEventModel.getProductItems().add(pdtProductItemDomainEventModel);
    pdtProductDomainEventModel.getImages().add(imageDomainEventModel);
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    pdtProductItemDomainEventModel.setDangerousGoodsLevel(1);

    AttributeDomainEventModel attributeDomainEventModel1 = new AttributeDomainEventModel();
    attributeDomainEventModel1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_1);
    attributeDomainEventModel1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    ProductAttributeDomainEventModel productAttributeDomainEventModel1 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel1.setAttribute(attributeDomainEventModel1);
    productAttributeDomainEventModel1.setProductAttributeValues(new ArrayList<>());
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel1 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel1.setAllowedAttributeValue(new AllowedAttributeValueDomainEventModel());
    productAttributeValueDomainEventModel1
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueDomainEventModel());

    productAttributeDomainEventModel1.getProductAttributeValues().add(productAttributeValueDomainEventModel1);
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(DEFAULT_SKU_CODE_2);
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    pdtProductDomainEventModel.setProductCategories(Arrays.asList(productCategoryDomainEventModel));
    AttributeDomainEventModel attributeDomainEventModel2 = new AttributeDomainEventModel();
    attributeDomainEventModel2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeDomainEventModel2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    ProductAttributeDomainEventModel productAttributeDomainEventModel2 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel2.setAttribute(attributeDomainEventModel2);
    productAttributeDomainEventModel2.setProductAttributeValues(new ArrayList<>());
    PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValueDomainEventModel =
        new PredefinedAllowedAttributeValueDomainEventModel();
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel2 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel2
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
    productAttributeDomainEventModel2.getProductAttributeValues().add(productAttributeValueDomainEventModel2);

    AttributeDomainEventModel attributeDomainEventModel4 = new AttributeDomainEventModel();
    attributeDomainEventModel4.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    attributeDomainEventModel4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    ProductAttributeDomainEventModel productAttributeDomainEventModel4 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel4.setAttribute(attributeDomainEventModel4);
    productAttributeDomainEventModel4.setProductAttributeValues(new ArrayList<>());
    PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValueDomainEventModel1 =
        new PredefinedAllowedAttributeValueDomainEventModel();
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel4 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel4
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel1);
    productAttributeDomainEventModel4.getProductAttributeValues().add(productAttributeValueDomainEventModel4);

    AttributeDomainEventModel attributeDomainEventModel5 = new AttributeDomainEventModel();
    attributeDomainEventModel5.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    attributeDomainEventModel5.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    AttributeDomainEventModel attributeDomainEventModel3 = new AttributeDomainEventModel();
    attributeDomainEventModel3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeDomainEventModel3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    ProductAttributeDomainEventModel productAttributeDomainEventModel3 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel3.setAttribute(attributeDomainEventModel3);
    productAttributeDomainEventModel3.setProductAttributeValues(new ArrayList<>());
    ProductAttributeDomainEventModel productAttributeDomainEventModel5 = new ProductAttributeDomainEventModel();
    productAttributeDomainEventModel5.setAttribute(attributeDomainEventModel5);
    productAttributeDomainEventModel5.setProductAttributeValues(new ArrayList<>());
    predefinedAllowedAttributeValueDomainEventModel =
        new PredefinedAllowedAttributeValueDomainEventModel();
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel3 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel3
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
    productAttributeDomainEventModel3.getProductAttributeValues().add(productAttributeValueDomainEventModel3);
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel5 =
        new ProductAttributeValueDomainEventModel();
    productAttributeValueDomainEventModel5
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDomainEventModel);
    productAttributeDomainEventModel5.getProductAttributeValues().add(productAttributeValueDomainEventModel5);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel1);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel2);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel3);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel4);
    pdtProductDomainEventModel.getProductAttributes().add(productAttributeDomainEventModel5);


    ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel =
        new ProductItemAttributeValueDomainEventModel();
    productItemAttributeValueDomainEventModel.setAttribute(attributeDomainEventModel1);
    productItemAttributeValueDomainEventModel.setValue(ITEM_ATTRIBUTE_VALUE_1);


    ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel1 =
        new ProductItemAttributeValueDomainEventModel();
    productItemAttributeValueDomainEventModel1.setAttribute(attributeDomainEventModel2);
    productItemAttributeValueDomainEventModel1.setValue(ITEM_ATTRIBUTE_VALUE_2);


    ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel2 =
        new ProductItemAttributeValueDomainEventModel();
    productItemAttributeValueDomainEventModel2.setAttribute(attributeDomainEventModel4);
    productItemAttributeValueDomainEventModel2.setValue(ITEM_ATTRIBUTE_VALUE_2);

    ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel3 =
        new ProductItemAttributeValueDomainEventModel();
    productItemAttributeValueDomainEventModel3.setAttribute(attributeDomainEventModel3);
    productItemAttributeValueDomainEventModel3.setValue(ITEM_ATTRIBUTE_VALUE_2);

    pdtProductDomainEventModel.getProductItems().get(0).setProductItemAttributeValues(Arrays
        .asList(productItemAttributeValueDomainEventModel, productItemAttributeValueDomainEventModel1,
            productItemAttributeValueDomainEventModel2, productItemAttributeValueDomainEventModel3));
    return pdtProductDomainEventModel;
  }

  private ProductRequest generateProductRequestForProcessImage(){
    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductCode(PRODUCT_CODE);
    productRequest.setActivated(true);
    productRequest.setViewable(true);
    productRequest.setVersion(1L);
    return productRequest;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    productCollection = new ProductCollection();
    productCollection.setId(PRODUCT_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    productDetailResponse = generateProductData();
    productBasicDetailsResponse = generateProductBasicDetailsResponse();
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "avoidDuplicateRescalingEnabled", false);
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.eq(Boolean.FALSE)))
        .thenReturn(productDetailResponse);
    Mockito.when(this.predefinedAttributeAllowedValueService
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new PredefinedAllowedAttributeValueResponse("test", 0, "10001"));
    Mockito.doNothing().when(this.approveProductService).approveContent(Mockito.any());
    Mockito.doNothing().when(this.approveProductService)
      .processImageWithScalingEligibility(Mockito.any(ProductRequest.class), Mockito.eq(0),
        Mockito.anyMap(), Mockito.anyMap());
    Mockito.doNothing().when(this.approveProductService).approveImage(Mockito.any(), Mockito.eq(0));
    Mockito.when(productWfService.status(Mockito.any())).thenReturn(
        new ProductWorkflowStatus(DEFAULT_SKU_CODE, Arrays.asList("CONTENT_APPROVAL", "IMAGE_APPROVAL"), null));
    Mockito.doNothing().when(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.doNothing().when(approveProductService).updateImage(Mockito.any(ProductRequest.class));
    Mockito.when(productService.findProductBasicDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productBasicDetailsResponse);
    profileResponse =
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(false)
        .build();
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(profileResponse);
    productBusinessPartnerCounter = new ProductBusinessPartnerCounter();
    productBusinessPartnerCounter.setAppealedProductCount(10);
    productBusinessPartnerCounter.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
      STORE_ID);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, productRequestArgumentCaptor.getValue().getBrandApprovalStatus());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerPdtProductDomainEventModelNullTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "avoidDuplicateRescalingEnabled", false);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setBrandApprovalStatus(null);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(null, productRequestArgumentCaptor.getValue().getBrandApprovalStatus());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerEditedTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setEdited(true);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE,false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerEdited1Test() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setEdited(true);
    request.setPostLive(true);
    request.setReviewPending(true);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerEdited2Test() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setEdited(true);
    request.setPostLive(true);
    request.setReviewPending(false);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerEdited3Test() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setEdited(true);
    request.setPostLive(false);
    request.setReviewPending(true);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerEdited4Test() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "avoidDuplicateRescalingEnabled", false);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setEdited(true);
    request.setPostLive(false);
    request.setReviewPending(false);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processVendorApprovalEventForEditedProductsTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, DEFAULT_SKU_CODE, false, null, null);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(xProductOutbound).updateContentChange(DEFAULT_SKU_CODE, true, true);
  }

  @Test
  public void processVendorApprovalEventForEditedProductsNoActivationNeededTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(StringUtils.EMPTY);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productBusinessPartnerService.getProductSkusByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(DEFAULT_SKU_CODE));
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(productBusinessPartnerService).getProductSkusByProductCode(PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(DEFAULT_SKU_CODE, null, false);
  }


  @Test
  public void processVendorApprovalEventForEditedProductsContentAndImageTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productBusinessPartnerService.getProductSkusByProductCode(PRODUCT_CODE)).thenReturn(Arrays.asList(PRODUCT_CODE));
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT_AND_IMAGE);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processEditedImage(productRequestArgumentCaptor.capture());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
  }

  @Test
  public void processVendorApprovalEventForEditedProductsContentAndImageInvalidTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT_AND_IMAGE);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processEditedImage(productRequestArgumentCaptor.capture());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
  }

  @Test
  public void processVendorApprovalEventForEditedProductsImageTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.IMAGE);
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processEditedImage(productRequestArgumentCaptor.capture());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
  }

  @Test
  public void processVendorApprovalEventForEditedProductsReviewPendingTrueTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setReviewPending(true);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.IMAGE);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerWithPromoSkuTrueTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPromoSKU(true);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertTrue(productRequestArgumentCaptor.getAllValues().get(0).isPromoSKU());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerPostLiveReviewDoneTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    request.setBrandCode(BRAND_CODE);
    request.setBrand(BRAND_NAME);
    request.setPostLive(true);
    request.setReviewPending(false);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(1).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(2).getProductCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, productRequestArgumentCaptor.getValue().getBrandApprovalStatus());
    Assertions.assertEquals(BRAND_NAME, productRequestArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(BRAND_CODE, productRequestArgumentCaptor.getValue().getBrandCode());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerPostLive_reviewedTestSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.eq(Boolean.FALSE)))
      .thenReturn(productDetailResponse);
    Image productImage = new Image();
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(false);
    itemImage2.setSequence(2);
    productDetailResponse.setImages(List.of(productImage,productImage2));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.setImages(Collections.singletonList(itemImage)));
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    request.setBrandCode(BRAND_CODE);
    request.setBrand(BRAND_NAME);
    request.setPostLive(true);
    request.setReviewPending(false);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImageAndAvoidHashCodeRegeneration(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService)
      .processImageWithScalingEligibility(productRequestArgumentCaptor.capture(), Mockito.eq(0),
        Mockito.anyMap(), Mockito.anyMap());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(1).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(2).getProductCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, productRequestArgumentCaptor.getValue().getBrandApprovalStatus());
    Assertions.assertEquals(BRAND_NAME, productRequestArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(BRAND_CODE, productRequestArgumentCaptor.getValue().getBrandCode());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumer_PostLiveReviewPendingTestWithSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService)
      .processImageWithScalingEligibility(productRequestArgumentCaptor.capture(), Mockito.eq(0),
        Mockito.anyMap(), Mockito.anyMap());
    Mockito.verify(approveProductService).approveImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(1).getProductCode());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processPDTaskQCKafkaConsumer_processImageStatusSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productWfService.status(PRODUCT_CODE)).thenReturn(
      new ProductWorkflowStatus(DEFAULT_SKU_CODE, Arrays.asList("PROCESS_IMAGE"), null));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(productWfService, Mockito.times(4)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(approveProductService).updateImageAndAvoidHashCodeRegeneration(Mockito.any(ProductRequest.class));
    Mockito.verify(approveProductService)
      .processImageWithScalingEligibility(productRequestArgumentCaptor.capture(), Mockito.eq(0),
        Mockito.anyMap(), Mockito.anyMap());
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChange_withItemAttributesChangeSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService)
      .processImageWithScalingEligibility(productRequestArgumentCaptor.capture(), Mockito.eq(0),
        Mockito.anyMap(), Mockito.anyMap());
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChange_withItemAttributesChangeSwitchOnMFDTrueImageTest() throws Exception {
    Mockito.when(productService.findProductBasicDetailByProductCode(PRODUCT_CODE))
      .thenReturn(productBasicDetailsResponse);
    productBasicDetailsResponse.getImages().forEach(image -> image.setMarkForDelete(true));
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService)
      .processImageWithScalingEligibility(productRequestArgumentCaptor.capture(), Mockito.eq(0),
        Mockito.anyMap(), Mockito.anyMap());
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }


  @Test
  public void processProductDistributionTaskQCKafkaConsumerPostLiveReviewDoneDGLevelTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(false);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(1).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(2).getProductCode());
    Assertions.assertEquals(DANGEROUS_GOODS_LEVEL_2,
        productRequestArgumentCaptor.getAllValues().get(0).getProductItems().get(0).getDangerousGoodsLevel());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerPostLiveReviewPendingTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, productRequestArgumentCaptor.getAllValues().get(1).getProductCode());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  private ProductItemDomainEventModel getProductItemDomainEventModel() {
    ProductItemDomainEventModel productItemDomainEventModel = new ProductItemDomainEventModel();
    productItemDomainEventModel.setActivated(true);
    productItemDomainEventModel.setDangerousGoodsLevel(0);
    productItemDomainEventModel.setViewable(true);
    productItemDomainEventModel.setSkuCode(DEFAULT_SKU_CODE);
    ImageDomainEventModel imageDomainEventModel = new ImageDomainEventModel();
    imageDomainEventModel.setSequence(0);
    imageDomainEventModel.setMainImage(true);
    imageDomainEventModel.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    productItemDomainEventModel.setImages(new ArrayList(Arrays.asList(imageDomainEventModel)));
    return productItemDomainEventModel;
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithContentExpiredTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    productBasicDetailsResponse.setVersion(VERSION + 1);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(Mockito.any(ProductRequest.class));
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService, Mockito.times(2)).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).clearMasterProductCacheSync(PRODUCT_ID, PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithNoDetails() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(this.productService.findProductDetailByProductCode(PRODUCT_CODE,Boolean.FALSE))
        .thenReturn(null);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productWfService).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.any(), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.eq(request.getProductCode()));
    Mockito.verify(productService)
        .saveProductHistory(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.DEFAULT_USERNAME,
            SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
    Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;


  @Test
  public void processPDTaskQCKafkaConsumerWithNoBasicDetails() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(this.productService.findProductBasicDetailByProductCode(PRODUCT_CODE)).thenReturn(null);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productWfService).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.any(), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.eq(request.getProductCode()));
    Mockito.verify(productService)
        .saveProductHistory(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.DEFAULT_USERNAME,
            SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
    Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerTestWithActiveProduct() throws Exception{
    PDTProductDomainEventModel productDomainEventModel = new PDTProductDomainEventModel();
    productDomainEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(productWfService.status(PRODUCT_CODE)).thenReturn(
        new ProductWorkflowStatus(DEFAULT_SKU_CODE, Arrays.asList("ACTIVE"), null));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(productDomainEventModel, 0);
    Mockito.verify(productWfService).status(PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithProcessImageStatusTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productWfService.status(PRODUCT_CODE)).thenReturn(
        new ProductWorkflowStatus(DEFAULT_SKU_CODE, Arrays.asList("PROCESS_IMAGE"), null));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(productWfService, Mockito.times(4)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(approveProductService).updateImage(Mockito.any(ProductRequest.class));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithApproveQCException() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productWfService.status(PRODUCT_CODE)).thenReturn(
        new ProductWorkflowStatus(DEFAULT_SKU_CODE, Arrays.asList("PROCESS_IMAGE"), null));
    Mockito.doThrow(new RuntimeException()).when(approveProductService).approveQc(PRODUCT_CODE);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(productWfService, Mockito.times(4)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(approveProductService).updateImage(Mockito.any(ProductRequest.class));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithExceptionTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productWfService.status(PRODUCT_CODE)).thenReturn(
        new ProductWorkflowStatus(DEFAULT_SKU_CODE, Arrays.asList("CONTENT_APPROVAL"), null));
    Mockito.doThrow(new RuntimeException()).when(approveProductService).approveContent(Mockito.any(ProductRequest.class));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(approveProductService).approveContent(Mockito.any(ProductRequest.class));
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(productWfService, Mockito.times(2)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.any(), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.eq(request.getProductCode()));
    Mockito.verify(productService)
        .saveProductHistory(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.DEFAULT_USERNAME,
            SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
    Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithItemAttributesChange() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithItemAttributesChangePredefinedAllowedAttributeValueNotNull() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setValue("test");
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
            .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
            .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
            .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
            .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
            .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
            .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(predefinedAttributeAllowedValueService, times(1))
            .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
            productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
            attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
                    .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
            .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
            productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithBrandValueSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", false);
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "fetchBrandValueByPredefinedAttributeValueCode", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setValue("test");
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(predefinedAttributeAllowedValueService, times(1))
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithBrandValueSwitchOn2() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "fetchBrandValueByPredefinedAttributeValueCode", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setValue("test");
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setPredefinedAllowedAttributeCode("test");
    request.setBrandCode("test");
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    attributeResponse.setName(BRAND);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
            request.getProductAttributes().get(1).getAttribute().getAttributeCode(), "test", true))
        .thenReturn(new PredefinedAllowedAttributeValueResponse("test", 0, "10001"));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(predefinedAttributeAllowedValueService)
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithBrandValueSwitchOn3() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "fetchBrandValueByPredefinedAttributeValueCode",
        true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setPredefinedAllowedAttributeCode("test");
    request.getProductAttributes().get(1).getAttribute().setName(BRAND);
    request.setBrandCode("test");
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    attributeResponse.setName(BRAND);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
            request.getProductAttributes().get(1).getAttribute().getAttributeCode(), "test", true))
        .thenReturn(new PredefinedAllowedAttributeValueResponse("test", 0, "10001"));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(productOutbound).getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
        request.getProductAttributes().get(1).getAttribute().getAttributeCode(), "test", true);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithBrandValueSwitchOnFallback() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "fetchBrandValueByPredefinedAttributeValueCode",
        true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setPredefinedAllowedAttributeCode("test");
    request.getProductAttributes().get(1).getAttribute().setName(BRAND);
    request.setBrandCode("test");
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    attributeResponse.setName(BRAND);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.doThrow(RuntimeException.class).when(productOutbound).getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
        request.getProductAttributes().get(1).getAttribute().getAttributeCode(), "test", true);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(predefinedAttributeAllowedValueService)
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(productOutbound).getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
        request.getProductAttributes().get(1).getAttribute().getAttributeCode(), "test", true);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryWithPredefinedSpecialAttributeValueDeletedExccemption2() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "specialAttributeValueCheckSwitch", false);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().forEach(productAttributeDomainEventModel -> productAttributeDomainEventModel.getAttribute().setSkuValue(false));
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    productDetailResponse.getProductAttributeResponses().forEach(productAttributeResponse -> productAttributeResponse.getAttribute().setSkuValue(false));
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.eq(Boolean.FALSE)))
        .thenReturn(productDetailResponse);
    Mockito.when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        Mockito.any(), Mockito.any())).thenThrow(Exception.class);
    try {
        this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    } finally {
      Mockito.verify(productWfService, Mockito.times(1)).status(PRODUCT_CODE);
      Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
      Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
      Mockito.verify(predefinedAttributeAllowedValueService, times(1))
          .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
      Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
      Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
      Mockito.verify(productService)
          .saveProductHistory(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
    }
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryWithPredefinedSpecialAttributeValueDeletedExccemption() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "specialAttributeValueCheckSwitch", false);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getAttribute().setSkuValue(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        Mockito.any(), Mockito.any())).thenThrow(Exception.class);
    try {
        this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    } finally {
      Mockito.verify(productWfService, Mockito.times(1)).status(PRODUCT_CODE);
      Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
      Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
      Mockito.verify(predefinedAttributeAllowedValueService, times(1))
          .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
      Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
      Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
      Mockito.verify(productService)
          .saveProductHistory(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
    }
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryWithPredefinedSpecialAttributeValueDeleted() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "specialAttributeValueCheckSwitch", true);
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "avoidDuplicateRescalingEnabled", false);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getAttribute().setSkuValue(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.doThrow(Exception.class).when(predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        Mockito.any(), Mockito.eq("test"));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(predefinedAttributeAllowedValueService, times(2))
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryWithPredefinedBrandAttributeValueDeleted() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "ignorePreDefinedAttributeValueCheckSwitch", true);
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "avoidDuplicateRescalingEnabled", false);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getAttribute().setSkuValue(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.when(predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        Mockito.any(), Mockito.any())).thenThrow(Exception.class);
    request.getProductAttributes().get(1).getAttribute().setName(BRAND);
    try {
        this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    } finally {
      Mockito.verify(productWfService, Mockito.times(1)).status(PRODUCT_CODE);
      Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
      Mockito.verify(productService).setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
      Mockito.verify(predefinedAttributeAllowedValueService, times(1))
          .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
      Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
      Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
      Mockito.verify(productService)
          .saveProductHistory(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
    }
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryWithPredefinedAttributeValueDeleted() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "ignorePreDefinedAttributeValueCheckSwitch", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getAttribute().setSkuValue(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.doThrow(Exception.class).when(predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        Mockito.any(), Mockito.eq("test"));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(predefinedAttributeAllowedValueService, times(1))
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }



  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryWithPredefinedSpecialAttributeValueDeleted2() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "specialAttributeValueCheckSwitch", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.getProductAttributes().get(1).getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
        .setValue("test");
    request.getProductAttributes().get(1).getAttribute().setSkuValue(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    Mockito.doThrow(Exception.class).when(predefinedAttributeAllowedValueService).findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(),
        Mockito.any(), Mockito.eq("test"));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(predefinedAttributeAllowedValueService, times(2))
        .findByStoreIdAndMatchAttributeCodeAndValue(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTruePreliveTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(false);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.verify(productService)
        .updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
            PRODUCT_CODE, false, SYSTEM_REVIEW);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTruePostliveTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.verify(productService)
        .updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
            PRODUCT_CODE, true, SYSTEM_REVIEW);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTrueAndSwitchOnBOPISTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "bopisCategoryActionOnCategoryChangeSwitch", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setProductType(ProductType.BOPIS.getProductType());
    request.getProductCategories().forEach(
      productCategoryDomainEventModel -> productCategoryDomainEventModel.getCategory()
        .setCategoryCode(CATEGORY_CODE));
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setBopisEligible(false);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(CATEGORY_CODE,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
    Mockito.verify(productService)
      .takeActionsOnCategoryChangeFromVendor(Constants.DEFAULT_STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
        PRODUCT_CODE, true, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTrueAndSwitchOnBOPISWithNonZeroDimensionsTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "bopisCategoryActionOnCategoryChangeSwitch", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setHeight(1.0);
    request.setWeight(2.0);
    request.setLength(2.0);
    request.setShippingWeight(2.0);
    request.setProductType(ProductType.BOPIS.getProductType());
    request.getProductCategories().forEach(
      productCategoryDomainEventModel -> productCategoryDomainEventModel.getCategory()
        .setCategoryCode(CATEGORY_CODE));
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setBopisEligible(false);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(CATEGORY_CODE,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
    Mockito.verify(productService)
      .takeActionsOnCategoryChangeFromVendor(Constants.DEFAULT_STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
        PRODUCT_CODE, false, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithMarginExceededTrueAndSwitchOnBOPISWithDiffCategoryTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "bopisCategoryActionOnCategoryChangeSwitch", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setProductType(ProductType.BOPIS.getProductType());
    request.getProductCategories().forEach(
      productCategoryDomainEventModel -> productCategoryDomainEventModel.getCategory()
        .setCategoryCode(CATEGORY_CODE));
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setBopisEligible(false);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound, times(2)).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
    Mockito.verify(productService)
      .takeActionsOnCategoryChangeFromVendor(Constants.DEFAULT_STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
        PRODUCT_CODE, true, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTrueAndSwitchOnNonBOPISTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "bopisCategoryActionOnCategoryChangeSwitch", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setProductType(ProductType.REGULAR.getProductType());
    request.getProductCategories().forEach(
      productCategoryDomainEventModel -> productCategoryDomainEventModel.getCategory()
        .setCategoryCode(CATEGORY_CODE));
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setBopisEligible(false);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(CATEGORY_CODE,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
    Mockito.verify(productService)
      .takeActionsOnCategoryChangeFromVendor(Constants.DEFAULT_STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
        PRODUCT_CODE, false, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTruePostliveAfterReviewTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(false);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Mockito.verify(productService)
        .updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
            PRODUCT_CODE, true, SYSTEM_REVIEW);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTruePostliveAfterReviewTestCategoryDeleted() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(false);
    productDetailResponse.getProductCategoryResponses().get(0).setMarkForDelete(true);
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.eq(Boolean.FALSE)))
        .thenReturn(productDetailResponse);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Mockito.verify(productService)
        .updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(STORE_ID, StringUtils.EMPTY,
          StringUtils.EMPTY,
            PRODUCT_CODE, true, SYSTEM_REVIEW);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededFalsePostliveAfterReviewTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(false);
    request.setPostLive(true);
    request.setReviewPending(false);
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.eq(Boolean.FALSE)))
        .thenReturn(productDetailResponse);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
        .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
        .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
        .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
        .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processImage(productRequestArgumentCaptor.capture(), Mockito.eq(0));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
        attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
        .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
        productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @AfterEach
  public void after(){
    verifyNoMoreInteractions(approveProductService, productWfService, productService,
        predefinedAttributeAllowedValueService, productStatusPublisherService, productOutbound, productLevel3Service,
        solrReviewProductCollectionService, xProductOutbound, productDistributionService, productAppealService);
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostliveTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(new Image()));
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(productDetailResponse);
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(approveProductService).processImageForRevisedProduct(Mockito.any(ProductDetailResponse.class));
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostLiveSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
      new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(new Image()));
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
      .thenReturn(productDetailResponse);
    this.productDistributionTaskQCServiceBean
      .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(approveProductService).processImageForRevisedProduct(Mockito.any(ProductDetailResponse.class));
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostliveTestImagesEmpty() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setImages(new ArrayList<>());
    request.setPdtImageDomainEventModels(new ArrayList<>());
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    productDetailResponse.setImages(new ArrayList<>());
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(productDetailResponse);
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(productWfService).approveImageForRevisedProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any());
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostliveTestImagesEmptyAutoApproval() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setImages(new ArrayList<>());
    request.setPdtImageDomainEventModels(new ArrayList<>());
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Collections.singletonList(productItemDomainEventModel1)));
    productDetailResponse.setImages(new ArrayList<>());
    productDetailResponse.setCategoryCodes(Collections.singletonList(PRODUCT_CODE));
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(productDetailResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    productCollection.setPostLive(true);
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(productWfService)
        .approveImageForRevisedProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
            Mockito.any());
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productCollectionRepository, times(2))
      .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productStatusPublisherService)
        .publishVendorCombinedEvent(autoApprovalTypeRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostliveTestImagesEmptyAutoApprovalFailure()
      throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    request.setImages(new ArrayList<>());
    request.setPdtImageDomainEventModels(new ArrayList<>());
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Collections.singletonList(productItemDomainEventModel1)));
    productDetailResponse.setImages(new ArrayList<>());
    productDetailResponse.setCategoryCodes(Collections.singletonList(PRODUCT_CODE));
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(productDetailResponse);
    Mockito.doThrow(RuntimeException.class).when(productCollectionRepository)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    productCollection.setPostLive(true);
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(productWfService)
        .approveImageForRevisedProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
            Mockito.any());
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForReviewPendingFalseTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "revisedProductScoreApi",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.getProductCategories().get(0).getCategory().setCategoryCode("CAT-1234");
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(false);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(new ProductDetailResponse());
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode("CAT-1234"))
      .thenReturn(categoryDetailResponse);
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productWfService).approveImageForRevisedProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(approveProductService).updateImage(Mockito.any(ProductRequest.class));
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode("CAT-1234");
    Mockito.verify(xProductOutbound)
      .generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForReviewPendingFalseEmptyCollectionTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.getProductItems().get(0).getPdtImageDomainEventModels().get(0).setActive(true);
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(false);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(new ProductDetailResponse());
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productWfService).approveImageForRevisedProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
        Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImage(Mockito.any(ProductRequest.class));
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
  }

  @Test
  public void processVendorApprovalEventForEditedPAnDRevisedProductTest() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setRevised(true);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.doNothing().when(productLevel3Service).activateProductOnNeedCorrection(STORE_ID, DEFAULT_SKU_CODE,
      profileResponse, new ArrayList<>());
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).activateProductOnNeedCorrection(Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_SKU_CODE),
        Mockito.eq(profileResponse), Mockito.any());
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(xProductOutbound).updateContentChange(DEFAULT_SKU_CODE, true, true);
  }

  @Test
  public void processVendorApprovalEventForEditedPAnDRevisedProductPreliveTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "checkEligibilityForEditedVendorApproval", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(false);
    request.setRevised(true);
    productCollection.setReviewPending(true);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(productCollection);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCode(Mockito.any(), Mockito.any()))
      .thenReturn(productCollection);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService, times(2)).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).activateProductOnNeedCorrection(Mockito.eq(STORE_ID), Mockito.eq(DEFAULT_SKU_CODE),
        Mockito.eq(profileResponse), Mockito.any());
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(xProductOutbound).updateContentChange(DEFAULT_SKU_CODE, true, true);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
  }

  @Test
  public void processVendorApprovalEventForEditedAndRevisedProductPreliveWithReviewPendingFalseTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "checkEligibilityForEditedVendorApproval", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(false);
    request.setRevised(true);
    productCollection.setReviewPending(false);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
      .thenReturn(productCollection);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCode(Mockito.any(), Mockito.any()))
      .thenReturn(productCollection);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
  }

  @Test
  public void processVendorApprovalEventForEditedAndRevisedProductPreliveWithReviewPendingFalseNullProductCollectionTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "checkEligibilityForEditedVendorApproval", true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(false);
    request.setRevised(true);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
      .thenReturn(null);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
  }


  @Test
  public void processVendorApprovalEventForEditedPAnDRevisedProductPreliveTest_emptySku() throws Exception {
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(false);
    request.setRevised(true);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(null);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(productCollection);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService, times(2)).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
  }

  @Test
  public void processAllImagesWithEligibilitySwitchOff() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", false);
    productDetailResponse = generateProductData();
    productDistributionTaskQCServiceBean.processAllImagesWithEligibility(0, productRequest,
      productDetailResponse);
    Mockito.verify(approveProductService).processImage(productRequest, 0);
  }

  @Test
  public void processAllImagesWithEligibilitySwitchOn() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    productDetailResponse = generateProductData();
    productRequest = generateProductRequestForProcessImage();
    Image productImage = new Image();
    productImage.setActive(true);
    productImage.setMarkForDelete(false);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    productDetailResponse.setImages(Collections.singletonList(productImage));
    productDetailResponse.getProductItemResponses().forEach(productItemResponse -> productItemResponse.setImages(Collections.singletonList(itemImage)));
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    productImageHashCodeMapDB = productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
      .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    Map<String, Image> finalItemImageHashCodeMapDB = itemImageHashCodeMapDB;
      productDetailResponse.getProductItemResponses().stream().findFirst().get().getImages()
      .forEach(image -> finalItemImageHashCodeMapDB.putIfAbsent(image.getHashCode(), image));
      productDistributionTaskQCServiceBean.processAllImagesWithEligibility(0, productRequest,
      productDetailResponse);
    approveProductService.processImageWithScalingEligibility(productRequest, 0,
      productImageHashCodeMapDB, itemImageHashCodeMapDB);
    Mockito.verify(approveProductService, times(2)).processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
  }

  @Test
  public void processAllImagesWithEligibilitySwitchOnMFDTrueImages() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    productDetailResponse = generateProductData();
    productRequest = generateProductRequestForProcessImage();
    Set<ProductItemResponse> productItemResponses = new HashSet<ProductItemResponse>();
    Image productImage = new Image();
    productImage.setActive(true);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(true);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    productDetailResponse.setImages(Collections.singletonList(productImage));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.setImages(Collections.singletonList(itemImage)));
    productDetailResponse.getImages().forEach(image -> image.setMarkForDelete(true));
    productDetailResponse.getProductItemResponses().stream().findFirst().get().getImages()
      .forEach(image -> image.setMarkForDelete(true));
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setSkuCode(PRODUCT_CODE);
    productItemResponse2.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    productItemResponse2.setImages(Collections.singletonList(itemImage));
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
    productImageHashCodeMapDB =
      productDetailResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
        .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    for (ProductItemResponse productItemResponse : productItemResponses) {
      productItemResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
        .forEach(image -> itemImageHashCodeMapDB.putIfAbsent(image.getHashCode(), image));
    }
    productDistributionTaskQCServiceBean.processAllImagesWithEligibility(0, productRequest,
      productDetailResponse);
    Mockito.verify(approveProductService).processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
  }

  @Test
  public void processAllImagesWithEligibilitySwitchOnOriginalImages() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    ProductDetailResponse productDetailResponse = generateProductData();
    ProductRequest productRequest = generateProductRequestForProcessImage();
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setSkuCode(DEFAULT_SKU_CODE);
    Image productImage = new Image();
    productImage.setActive(true);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(true);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH).concat(RESIZE));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(true);
    itemImage2.setSequence(2);
    productDetailResponse.setImages(List.of(productImage,productImage2));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.setImages(Collections.singletonList(itemImage)));
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setSkuCode(PRODUCT_CODE);
    productItemResponse2.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    productItemResponse2.setImages(Collections.singletonList(itemImage2));
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
    productRequest.setImages(productDetailResponse.getImages());
    productItemRequest.setImages(Arrays.asList(itemImage,itemImage2));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    itemImageHashCodeMapDB =
      Stream.of(itemImage2, itemImage).filter(this::isImageValidForScaling).distinct()
        .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    productDistributionTaskQCServiceBean.processAllImagesWithEligibility(0, productRequest,
      productDetailResponse);
    productImageHashCodeMapDB =
      Stream.of(productImage2,productImage).filter(this::isImageValidForScaling).distinct()
        .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    Mockito.verify(approveProductService).processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
  }

  @Test
  public void processAllImagesWithEligibilitySwitchOnResizedImages() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    ProductDetailResponse productDetailResponse = generateProductData();
    ProductRequest productRequest = generateProductRequestForProcessImage();
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setSkuCode(DEFAULT_SKU_CODE);
    Image productImage = new Image();
    productImage.setActive(true);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    productImage2.setActive(true);
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH).concat(RESIZE));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(false);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(true);
    itemImage2.setSequence(2);
    productDetailResponse.setImages(List.of(productImage,productImage2));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.setImages(Collections.singletonList(itemImage)));
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setSkuCode(PRODUCT_CODE);
    productItemResponse2.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    productItemResponse2.setImages(Collections.singletonList(itemImage2));
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
    productRequest.setImages(productDetailResponse.getImages());
    productItemRequest.setImages(Arrays.asList(itemImage,itemImage2));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    itemImageHashCodeMapDB =
      Stream.of(itemImage2, itemImage).filter(this::isImageValidForScaling).distinct()
      .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    productDistributionTaskQCServiceBean.processAllImagesWithEligibility(0, productRequest,
      productDetailResponse);
    productImageHashCodeMapDB =
      Stream.of(productImage2,productImage).filter(this::isImageValidForScaling).distinct()
      .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    Mockito.verify(approveProductService).processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
  }

  @Test
  public void processAllImagesWithEligibilitySwitchOnInActiveImages() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    ProductDetailResponse productDetailResponse = generateProductData();
    ProductRequest productRequest = generateProductRequestForProcessImage();
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setSkuCode(DEFAULT_SKU_CODE);
    Image productImage = new Image();
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(false);
    itemImage2.setSequence(2);
    productDetailResponse.setImages(List.of(productImage,productImage2));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.setImages(Collections.singletonList(itemImage)));
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setSkuCode(PRODUCT_CODE);
    productItemResponse2.setDangerousGoodsLevel(DEFAULT_DANGEROUS_GOODS_LEVEL);
    productItemResponse2.setImages(Collections.singletonList(itemImage2));
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
    productRequest.setImages(productDetailResponse.getImages());
    productItemRequest.setImages(Arrays.asList(itemImage,itemImage2));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    itemImageHashCodeMapDB =
      Stream.of(itemImage2, itemImage).filter(this::isImageValidForScaling).distinct()
        .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    productDistributionTaskQCServiceBean.processAllImagesWithEligibility(0, productRequest,
      productDetailResponse);
    productImageHashCodeMapDB =
      Stream.of(productImage2,productImage).filter(this::isImageValidForScaling).distinct()
        .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
    Mockito.verify(approveProductService).processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
  }

  @Test
  public void processPDTaskQCKafkaConsumerWithCategoryChangeWithMarginExceededTruePostliveAfterReviewTestSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    Mockito.when(this.productService.findProductDetailByProductCode(Mockito.any(), Mockito.eq(Boolean.FALSE)))
      .thenReturn(productDetailResponse);
    Image productImage = new Image();
    productImage.setActive(true);
    productImage.setMarkForDelete(false);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    productDetailResponse.setImages(Collections.singletonList(productImage));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productDetailResponse.setImages(Collections.singletonList(itemImage)));
    PDTProductDomainEventModel request = generatePdtProductDomainEventModelForCategoryChange();
    request.setMarginExceeded(true);
    request.setPostLive(true);
    request.setReviewPending(false);
    Map<String, String> hashCodeXLocations  = new HashMap<>();
    Image image5 = new Image();
    image5.setActive(false);
    image5.setMainImages(false);
    image5.setMarkForDelete(false);
    image5.setSequence(2);
    image5.setHashCode("2d8a10ba8819dd1caea27e74e6aa9063");
    image5.setCommonImage(true);
    image5.setLocationPath("product-image");
    image5.setOriginalImage(null);
    image5.setEdited(false);
    Image image4 = new Image();
    image4.setActive(false);
    image4.setMainImages(false);
    image4.setMarkForDelete(false);
    image4.setSequence(2);
    image4.setHashCode("d2cfabc9a63f68a0225e307dbb7cae8e");
    image4.setCommonImage(true);
    image4.setLocationPath("item-image");
    image4.setOriginalImage(null);
    image4.setEdited(false);
    List<Image> modifiableList = new ArrayList<>(
      productDetailResponse.getProductItemResponses().iterator().next().getImages());
    modifiableList.add(image4);
    productDetailResponse.getProductItemResponses()
      .forEach(productItemResponse -> productItemResponse.setImages(modifiableList));
    List<Image> images = new ArrayList<>(productDetailResponse.getImages());
    images.add(image5);
    productDetailResponse.setImages(images);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    categoryDetailResponse.setCatalog(catalogResponse);
    categoryDetailResponse.setCategoryCode(DEFAULT_SKU_CODE_2);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_5);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2))
      .thenReturn(categoryDetailResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_4))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_5))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_1))
      .thenReturn(attributeResponse1);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_2))
      .thenReturn(attributeResponse);
    Mockito.when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_3))
      .thenReturn(attributeResponse);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).updateImageAndAvoidHashCodeRegeneration(productRequestArgumentCaptor.capture());
    Mockito.verify(approveProductService).processImageWithScalingEligibility(productRequestArgumentCaptor.capture(),
      Mockito.eq(0), Mockito.anyMap(),Mockito.anyMap());
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productOutbound, times(5)).getAttributeDetailByAttributeCode(Mockito.any());
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(DEFAULT_SKU_CODE_2);
    Mockito.verify(productService).checkIfProductWasTakeDown(productDetailResponse, PRODUCT_ID);
    Mockito.verify(productService)
      .updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(STORE_ID, DEFAULT_SKU_CODE, DEFAULT_SKU_CODE,
        PRODUCT_CODE, true, SYSTEM_REVIEW);
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductCategories().size());
    Assertions.assertEquals(DEFAULT_SKU_CODE_2,
      productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(5, productRequestArgumentCaptor.getValue().getProductAttributes().size());
    Assertions.assertEquals(2, productRequestArgumentCaptor.getValue().getProductAttributes().stream().filter(
      attribute -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
        .equals(attribute.getAttribute().getAttributeType().name())).count());
    Assertions.assertEquals(1, productRequestArgumentCaptor.getValue().getProductAttributes().stream()
      .filter(attribute -> DEFAULT_ATTRIBUTE_CODE_2.equals(attribute.getAttribute().getAttributeCode())).count());
    Assertions.assertEquals(4,
      productRequestArgumentCaptor.getValue().getProductItems().get(0).getProductItemAttributeValues().size());
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, true);
  }

  @Test
  public void testUpdateProductImageRequest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    ProductDetailResponse productDetailResponse = generateProductData();
    Image image4 = new Image();
    image4.setActive(false);
    image4.setMainImages(false);
    image4.setMarkForDelete(false);
    image4.setSequence(2);
    image4.setHashCode("d2cfabc9a63f68a0225e307dbb7cae8e");
    image4.setCommonImage(true);
    image4.setLocationPath("item-image");
    image4.setOriginalImage(null);
    image4.setEdited(false);
    Image image5 = new Image();
    image5.setActive(false);
    image5.setMainImages(false);
    image5.setMarkForDelete(false);
    image5.setSequence(2);
    image5.setHashCode("2d8a10ba8819dd1caea27e74e6aa9063");
    image5.setCommonImage(true);
    image5.setLocationPath("product-image");
    image5.setOriginalImage(null);
    image5.setEdited(false);
    List<Image> images = new ArrayList<>(productDetailResponse.getImages());
    images.add(image5);
    productDetailResponse.setImages(images);
    List<Image> modifiableList = new ArrayList<>(
      productDetailResponse.getProductItemResponses().iterator().next().getImages());
    modifiableList.add(image4);
    productDetailResponse.getProductItemResponses()
      .forEach(productItemResponse -> productItemResponse.setImages(modifiableList));
    ProductRequest productRequest = generateProductRequestForProcessImage();
    Image productImage = new Image();
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(false);
    itemImage2.setSequence(2);
    productRequest.setImages(Arrays.asList(productImage, image5));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(Arrays.asList(itemImage2));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    Map<String, String> result =
      productDistributionTaskQCServiceBean.updateProductImageRequest(productDetailResponse,
        productRequest);

  }

  @Test
  public void testUpdateProductImageRequest2() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    ProductDetailResponse productDetailResponse = generateProductData();
    productDetailResponse.getImages().forEach(image -> image.setOriginalImage(false));
    productDetailResponse.getProductItemResponses().iterator().next().getImages().forEach(image -> image.setOriginalImage(false));
    Image image4 = new Image();
    image4.setActive(false);
    image4.setMainImages(false);
    image4.setMarkForDelete(false);
    image4.setSequence(2);
    image4.setHashCode("d2cfabc9a63f68a0225e307dbb7cae8e");
    image4.setCommonImage(true);
    image4.setLocationPath("item-image");
    image4.setOriginalImage(true);
    image4.setEdited(false);
    Image image5 = new Image();
    image5.setActive(false);
    image5.setMainImages(false);
    image5.setMarkForDelete(false);
    image5.setSequence(2);
    image5.setHashCode("2d8a10ba8819dd1caea27e74e6aa9063");
    image5.setCommonImage(true);
    image5.setLocationPath("product-image");
    image5.setOriginalImage(true);
    image5.setEdited(false);
    List<Image> images = new ArrayList<>(productDetailResponse.getImages());
    images.add(image5);
    productDetailResponse.setImages(images);
    List<Image> modifiableList = new ArrayList<>(
      productDetailResponse.getProductItemResponses().iterator().next().getImages());
    modifiableList.add(image4);
    productDetailResponse.getProductItemResponses()
      .forEach(productItemResponse -> productItemResponse.setImages(modifiableList));
    ProductRequest productRequest = generateProductRequestForProcessImage();
    Image productImage = new Image();
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(false);
    itemImage2.setSequence(2);
    productRequest.setImages(Arrays.asList(productImage, image5));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(Arrays.asList(itemImage2));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    Map<String, String> result =
      productDistributionTaskQCServiceBean.updateProductImageRequest(productDetailResponse,
        productRequest);

  }

  @Test
  public void testUpdateProductImageRequestWithResized() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean,
      "avoidDuplicateRescalingEnabled", true);
    ProductDetailResponse productDetailResponse = generateProductData();
    productDetailResponse.getImages().forEach(image -> image.setLocationPath(image.getLocationPath().concat(RESIZE)));
    productDetailResponse.getProductItemResponses().iterator().next().getImages().forEach(image -> image.setLocationPath(image.getLocationPath().concat(RESIZE)));
    Image image4 = new Image();
    image4.setActive(false);
    image4.setMainImages(false);
    image4.setMarkForDelete(false);
    image4.setSequence(2);
    image4.setHashCode("d2cfabc9a63f68a0225e307dbb7cae8e");
    image4.setCommonImage(true);
    image4.setLocationPath("item-image");
    image4.setOriginalImage(true);
    image4.setEdited(false);
    Image image5 = new Image();
    image5.setActive(false);
    image5.setMainImages(false);
    image5.setMarkForDelete(false);
    image5.setSequence(2);
    image5.setHashCode("2d8a10ba8819dd1caea27e74e6aa9063");
    image5.setCommonImage(true);
    image5.setLocationPath("product-image");
    image5.setOriginalImage(true);
    image5.setEdited(false);
    List<Image> images = new ArrayList<>(productDetailResponse.getImages());
    images.add(image5);
    productDetailResponse.setImages(images);
    List<Image> modifiableList = new ArrayList<>(
      productDetailResponse.getProductItemResponses().iterator().next().getImages());
    modifiableList.add(image4);
    productDetailResponse.getProductItemResponses()
      .forEach(productItemResponse -> productItemResponse.setImages(modifiableList));
    ProductRequest productRequest = generateProductRequestForProcessImage();
    Image productImage = new Image();
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setHashCode(PRODUCT_IMAGE_HASH_1);
    productImage.setSequence(1);
    productImage.setOriginalImage(false);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImage2 = new Image();
    productImage2.setMarkForDelete(false);
    productImage2.setHashCode(PRODUCT_IMAGE_HASH_1.concat(PRODUCT_ID));
    productImage2.setOriginalImage(false);
    productImage2.setLocationPath(PRODUCT_IMAGE_LOCATION.concat(PRODUCT_ID));
    Image itemImage = new Image();
    itemImage.setCommonImage(true);
    itemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION.concat(Constants.DELIMITER_SLASH));
    itemImage.setHashCode(ITEM_IMAGE_HASH_1);
    itemImage.setMarkForDelete(false);
    itemImage.setActive(true);
    itemImage.setSequence(2);
    Image itemImage2 = new Image();
    itemImage2.setCommonImage(true);
    itemImage2.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    itemImage2.setHashCode(ITEM_IMAGE_HASH_1.concat(PRODUCT_ID));
    itemImage2.setMarkForDelete(false);
    itemImage2.setActive(false);
    itemImage2.setSequence(2);
    productRequest.setImages(Arrays.asList(productImage, image5));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(Arrays.asList(itemImage2));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    Map<String, String> result =
      productDistributionTaskQCServiceBean.updateProductImageRequest(productDetailResponse,
        productRequest);

  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerOverridePostLiveTrueTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setAppealedProduct(true);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setStoreId(STORE_ID);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    Mockito.when(productAppealService.fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(Pair.of(10, productBusinessPartnerCounter));
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, productRequestArgumentCaptor.getValue().getBrandApprovalStatus());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAppealService).fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(productAppealService).decrementCounterForProductAppeal(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processProductDistributionTaskQCKafkaConsumerOverridePostLiveTrueProductCollectionNullTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(null);
    this.productDistributionTaskQCServiceBean.processProductDistributionTaskQCKafkaConsumer(request, 0);
    Mockito.verify(approveProductService).approveQc(PRODUCT_CODE);
    Mockito.verify(approveProductService).processImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveImage(Mockito.any(ProductRequest.class), Mockito.eq(0));
    Mockito.verify(approveProductService).approveContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productWfService, Mockito.times(3)).status(PRODUCT_CODE);
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, productRequestArgumentCaptor.getValue().getBrandApprovalStatus());
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostLiveOverridePostLiveTrueTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
      new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(false);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(new Image()));
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setAppealedProduct(true);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setStoreId(STORE_ID);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
      .thenReturn(productDetailResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    Mockito.when(productAppealService.fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(Pair.of(10, productBusinessPartnerCounter));
    this.productDistributionTaskQCServiceBean
      .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(approveProductService).processImageForRevisedProduct(Mockito.any(ProductDetailResponse.class));
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productAppealService).fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(productAppealService).decrementCounterForProductAppeal(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productDistributionService)
      .removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostLiveOverridePostLiveTrueNotAppealedTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
        true);
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "revisedProductScoreApi",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
        new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(false);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(new Image()));
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setAppealedProduct(false);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setStoreId(STORE_ID);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
        .thenReturn(productDetailResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    this.productDistributionTaskQCServiceBean
        .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(approveProductService).processImageForRevisedProduct(Mockito.any(ProductDetailResponse.class));
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
        Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productDistributionService)
      .removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(approveProductService).updateImage(productRequestArgumentCaptor.capture());
  }

  @Test
  public void processVendorApprovalEventForRevisedProductsForPostLiveOverridePostLiveTrueProductCollectionNullTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    request.setPostLive(true);
    request.setReviewPending(true);
    PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel =
      new PDTRevisedProductVendorApprovedEventModel();
    pdtRevisedProductVendorApprovedEventModel.setPostLive(true);
    pdtRevisedProductVendorApprovedEventModel.setReviewPending(true);
    ProductItemDomainEventModel productItemDomainEventModel = getProductItemDomainEventModel();
    PDTProductItemDomainEventModel productItemDomainEventModel1 = new PDTProductItemDomainEventModel();
    BeanUtils.copyProperties(productItemDomainEventModel, productItemDomainEventModel1, "productItemAttributeValues");
    request.setProductItems(new ArrayList(Arrays.asList(productItemDomainEventModel1)));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(new Image()));
    Mockito.when(productOutbound.getImagesForScalingByProductCode(Mockito.any()))
      .thenReturn(productDetailResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(null);
    this.productDistributionTaskQCServiceBean
      .processVendorApprovalEventForRevisedProducts(request, pdtRevisedProductVendorApprovedEventModel);
    Mockito.verify(productOutbound).getImagesForScalingByProductCode(Mockito.any());
    Mockito.verify(productService).findProductDetailByProductCode(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(productService).findProductBasicDetailByProductCode(Mockito.any());
    Mockito.verify(approveProductService).processImageForRevisedProduct(Mockito.any(ProductDetailResponse.class));
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE,
      Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void processVendorApprovalEventForEditedProductsOverridePostLiveFlagTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setAppealedProduct(true);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setStoreId(STORE_ID);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(productBusinessPartner);
    Mockito.when(productAppealService.fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(Pair.of(10, productBusinessPartnerCounter));
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, DEFAULT_SKU_CODE, false, null, null);
    Mockito.verify(this.kafkaProducer)
      .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(xProductOutbound).updateContentChange(DEFAULT_SKU_CODE, true, true);
    Mockito.verify(productAppealService).fetchThresholdAndCounterForAppealProduct(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(productAppealService).decrementCounterForProductAppeal(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(productBusinessPartnerService).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);

  }

  @Test
  public void processVendorApprovalEventForEditedProductsOverridePostLiveFlagProductCollectionNullTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskQCServiceBean, "overridePostLiveFlag",
      true);
    PDTProductDomainEventModel request = generatePdtProductDomainEventModel();
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_ID)).thenReturn(DEFAULT_SKU_CODE);
    Mockito.when(productService.updateReviewType(STORE_ID, PRODUCT_CODE, null)).thenReturn(productCollection);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(null);
    this.productDistributionTaskQCServiceBean.processVendorApprovalEventForEditedProducts(request, ProductDistributionTaskQCServiceBean.CONTENT);
    Mockito.verify(productService).updateProductContent(productRequestArgumentCaptor.capture());
    Mockito.verify(productOutbound).republishProduct(Mockito.any(), Mockito.anyList());
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_ID);
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(productDistributionService).removeProductFromPDT(Mockito.any(), Mockito.any(), Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).findProductDetailByProductCode(PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, DEFAULT_SKU_CODE, false, null, null);
    Mockito.verify(this.kafkaProducer)
      .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(xProductOutbound).updateContentChange(DEFAULT_SKU_CODE, true, true);
  }

  @Test
  public void resetAndDecreaseCounterInternalBpCodeTest() throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
      .thenReturn(new ProductBusinessPartner());
    productCollection.setBusinessPartnerCode(Constants.INTERNAL);
    productDistributionTaskQCServiceBean.resetAndDecreaseCounter(productCollection);
    Mockito.verify(productBusinessPartnerService)
      .findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void resetAndDecreaseCounterNullL3Test() throws Exception {
    productCollection.setStoreId(STORE_ID);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
      .thenReturn(null);
    productDistributionTaskQCServiceBean.resetAndDecreaseCounter(productCollection);
    Mockito.verify(productBusinessPartnerService)
      .findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }


}
