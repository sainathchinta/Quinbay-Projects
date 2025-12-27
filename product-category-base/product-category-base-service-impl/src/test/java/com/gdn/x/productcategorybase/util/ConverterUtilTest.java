package com.gdn.x.productcategorybase.util;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.MinWholesaleDiscountDTO;
import com.gdn.x.productcategorybase.dto.ProductAttributeDTO;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.WholesaleConfigDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.CopyImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.dto.response.BrandSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeAndNameResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.PredictionCategoryMapping;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import com.gdn.x.productcategorybase.entity.RestrictedKeywordHistory;
import com.gdn.x.productcategorybase.entity.WholesalePriceConfiguration;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.service.FileStorageService;

public class ConverterUtilTest {

  private static final String CREATED_BY = "createdBy";
  private static final String MERCHANT_NAME = "BliBli";
  private static final String UPDATED_BY = "updatedBy";
  private static final String BRAND_CODE = "BR-00001";
  private static final String ID = "ID";
  private static final String DEFAULT_DESCRIPTION = "DESCRIPTION";
  private static final String CATEGORY_CODE = "CAT-01";
  private static final String CATEGORY_CODE_1 = "CAT-02";
  private static final String CATEGORY_NAME = "CAT-NAME";
  private static final String USERNAME = "username";
  private static final String USER_NAME = "system";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String OLD_VALUE = "Pre-live";
  private static final String NEW_VALUE = "Post-live";
  private static final String ACTIVITY = "Registered";
  private static final String STORE_ID = "10001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String ATTRIBUTE_CODE = "ATT_";
  private static final String ATTRIBUTE_CODE_2 = "ATT_2";
  private static final String VALUE = "Default_Value";
  private static final String VALUE2 = "Default_Value_2";
  private static final String SKU_CODE = "sku_code";
  private static final String ITEM_NAME = "item_name";
  private static final String PRODUCT_CODE = "MTA-000001";
  private static final String BRAND_LOGO = "brandLogo.logo";
  private static final String BRAND_LOGO_PATH = "/BR-00001/brandLogo.logo";
  private static final String DEFAULT_BRAND_CODE = "BRD-00000";
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String BRAND_WIP_ID = "brandWipId";
  private static final String DEFAULT_SELLER_CODE = "seller code";
  private static final String PREDICTION_ID = "predictionId";
  private static final String PREDICTION_ID_1 = "predictionId1";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static final int ACTION = 1 ;
  private static final String DESTINATON_CATEGORY = "destination_category";
  private static final String TYPE = "type";
  private static final String MESSAGE = "message";
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "-";
  private static final String ALLOWED_ATTRIBUTE_VALUE = "xiomi";
  private static final String PREDEFINED_ATTRIBUTE_VALUE = "XL";

  private CategoryRestrictedKeyword categoryRestrictedKeyword = new CategoryRestrictedKeyword();
  private RestrictedKeyword restrictedKeyword = new RestrictedKeyword();

  private MerchantConfigurationHistory merchantConfigurationHistory = new MerchantConfigurationHistory();
  private static final String MAIL_EVENT = "Category";
  private static final String PRODUCT_ID = "productId";
  private static final String LOCATION_PATH = "MTA-0000001/locationPath.jpeg";
  private static final String LOCATION_PATH2 = "MTA-0000001/locationPath2.jpeg";
  private static final String LOCATION_PATH3 = "MTA-0000001/locationPath3.jpeg";
  private static final String LOCATION_PATH4 = "MTA-0000001/locationPath4.jpeg";
  private static final String LOCATION_PATH5 = "MTA-0000001/locationPath5.jpeg";
  private static final String LOCATION_PATH6 = "MTA-0000001/locationPath6.jpeg";
  private static final String LOCATION_PATH7 = "MTA-0000001/locationPath7.jpeg";
  private static final String LOCATION_PATH8 = "MTA-0000001/locationPath8.jpeg";
  private static final String LOCATION_PATH9 = "MTA-0000001/locationPath9.jpeg";
  private static final String LOCATION_PATH10 = "MTA-0000001/locationPath10.jpeg";
  private static final String LOCATION_PATH11 = "MTA-0000001/locationPath11.jpeg";
  private static final String LOCATION_PATH12 = "MTA-0000001/locationPath12.jpeg";
  private static final String LOCATION_PATH13 = "MTA-0000001/locationPath13.jpeg";
  private static final String LOCATION_PATH14 = "MTA-0000001/locationPath14.jpeg";
  private static final String LOCATION_PATH15 = "MTA-0000001/locationPath15.jpeg";
  private static final String LOCATION_PATH16 = "MTA-0000001/locationPath16.jpeg";
  private static final String IMAGE_NAME = "locationPath";
  private static final String IMAGE_NAME1 = "location1Path";
  private static final String PRODUCT_NAME = "productName";
  private static final String BRAND = "brand";
  private static final String ORIGINAL_BRAND = "originalBrand";
  private static final Double ORIGINAL_LENGTH = 2.0;
  private static final Double ORIGINAL_WIDTH = 2.0;
  private static final Double ORIGINAL_HEIGHT = 2.0;
  private static final Double ORIGINAL_WEIGHT = 2.0;
  private static final Double ORIGINAL_SHIPPING_WEIGHT = 2.0;
  private static final String BRAND_ATTRIBUTE_NAME = "Brand";
  private static final String CATALOG_CODE = "catalog";
  private static final String DESCRIPTIVE_VALUE = "descriptive_value";
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD1 = "keyword1";
  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD_ID1 = "keywordId1";
  private static final String OLD_SALES_CATEGORY = "old_sales";
  private static final String NEW_SALES_CATEGORY = "new_sales";
  private static final String OLD_B2B_SALES_CATEGORY = "old_b2b_sales";
  private static final String NEW_B2B_SALES_CATEGORY = "new_b2b_sales";
  private static final Integer QUANTITY = 10;
  private static final int PAGE = 0;
  private static final int SIZE = 50;
  private static final Double PERCENTAGE = 10D;
  private static final Double PRICE = 100000D;
  private static final String CONFIGURATIONTYPE = "PERCENTAGE";
  private static final String WHOLESALECONFIG =
    "[{\"quantity\":10,\"minWholesaleDiscount\":" + "[{\"price\":100000.0,\"percentage\":10.0}]}]";
  private static final String RESIZE_LOCATION_PATH = "MTA-0000001/resize/locationPath.jpg";
  private static final String RESIZE_LOCATION_PATH1 = "MTA-0000001/resize/location1Path";
  private static final String LOCATION_PATH1 = "MTA-0000001/location1Path.jpeg";
  private static final String OSC_ID = "OSC_ID";
  private static final String OSC_CODE = "OSC_CODE";
  private static final String OSC_SHORT_TEXT = "OSC_SHORT_TEXT";
  private static final String OSC_LONG_TEXT = "OSC_LONG_TEXT";
  private static final String UNIQUE_SELLING_POINT = "usp";
  private static final String DESCRIPTION = "description";
  private static final String BRAND_NAME = "brand-name";
  private static final String DOCUMENT_LINKS = "document1.pdf,document2.pdf";
  private static final String SIZE_CHART_DELIMITER = "-";
  private Brand brand;
  private BrandWip brandWip;
  private BrandApproveRequest brandApproveRequest;
  private Page<BrandWip> page;
  private List<BrandWip> brandWipList = new ArrayList<>();
  private BrandWipHistory brandWipHistory = new BrandWipHistory();
  private List<BrandWipHistory> brandWipHistoryList = new ArrayList<>();
  private Page<BrandWipHistory> brandWips;
  private CategoryConfiguration categoryConfiguration = new CategoryConfiguration();
  private Category category = new Category();
  private CategoryConfigurationHistory categoryConfigurationHistory =
    new CategoryConfigurationHistory();
  private MerchantConfiguration merchantConfiguration = new MerchantConfiguration();
  private Attribute attribute = new Attribute();
  private ProductItem productItem = new ProductItem();
  private Product product = new Product();
  private ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
  private PredefinedAllowedAttributeValue originalBrandAttribute =
    new PredefinedAllowedAttributeValue();
  private ProductAttribute productAttribute = new ProductAttribute();
  private ProductAttributeValue productAttributeValue = new ProductAttributeValue();
  private ProductImage productImage = new ProductImage();
  private ProductImage productImage2 = new ProductImage();
  private ProductImage productImage3 = new ProductImage();
  private ProductItemImage productItemImage = new ProductItemImage();
  private ProductItemImage productItemImage2 = new ProductItemImage();
  private ProductItemImage productItemImage3 = new ProductItemImage();
  private ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
  private Image image = new Image();
  private PredefinedAllowedAttributeValue updatedBrandAttribute;
  private ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
  private ProductCategory productCategory = new ProductCategory();
  private ProductCategory productCategory1 = new ProductCategory();
  private Catalog catalog = new Catalog();
  private AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
  private CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
  private CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO2 = new CategoryKeywordsUpdateDTO();
  private CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO =
    new CategoryKeywordsUpdateListDTO();
  private Map<String, RestrictedKeyword> keywordMap = new HashMap<>();
  private ProductSalesCategoryMapping productSalesCategoryMapping =
    new ProductSalesCategoryMapping();
  private MinWholesaleDiscountDTO minWholesaleDiscountDTO;
  private WholesaleConfigDTO wholesaleConfigDTO;
  private WholesaleMappingDTO wholesaleMappingDTO;
  private OriginalSalesCategory originalSalesCategory;
  private BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
  private BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
  private Page<BrandAuthorisationHistory> brandAuthHistory;
  private List<BrandAuthorisationHistory> brandAuthHistoryList = new ArrayList<>();
  private CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponse;
  private Dimension dimension;
  private DimensionMapping dimensionMapping;
  private Page<DimensionMapping> dimensionMappingPage;
  private List<DimensionMapping> dimensionMappingList;

  @Mock
  private FileStorageService fileStorageService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    categoryRestrictedKeywordResponse = new CategoryRestrictedKeywordResponse();
    brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    brandWip = new BrandWip();
    brandWip.setStoreId(STORE_ID);
    brandWip.setBrandCode(BRAND_CODE);
    brandWip.setBrandRequestCode(BRAND_CODE);
    brandWip.setState(BrandWipState.DRAFT);
    brandWip.setBrandLogoPath(BRAND_LOGO);
    brandWip.setBrandName(BRAND);
    brandWip.setBusinessPartnerCode(MERCHANT_CODE);

    brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    brandWipHistory.setBrandRequestCode(BRAND_CODE);
    brandWipHistory.setBrandRequestCode(BRAND_CODE);
    brandWipHistory.setDescription(DEFAULT_DESCRIPTION.getBytes());
    brandWipHistory.setState(BrandWipState.APPROVED);
    brandWipHistoryList.add(brandWipHistory);
    brandWips = new PageImpl<>(brandWipHistoryList);
    brandWip.setBrandDescription(DEFAULT_DESCRIPTION.getBytes());
    brandWipList.add(brandWip);
    page = new PageImpl<>(brandWipList);

    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME);
    category.setOscUpdatedBy(UPDATED_BY);
    category.setOscUpdatedDate(new Date());
    categoryConfiguration.setCategory(category);
    categoryConfiguration.setCreatedDate(new Date());
    categoryConfiguration.setCreatedBy(USERNAME);
    categoryConfiguration.setReviewConfig(Constants.PRE_LIVE_STATUS);

    merchantConfiguration.setCreatedDate(new Date());
    merchantConfiguration.setCreatedBy(CREATED_BY);
    merchantConfiguration.setMerchantCode(MERCHANT_CODE);
    merchantConfiguration.setMerchantName(MERCHANT_NAME);
    merchantConfiguration.setCategoryName(CATEGORY_NAME);
    merchantConfiguration.setReviewConfig(Constants.PRE_LIVE_STATUS);

    categoryConfigurationHistory.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationHistory.setOldValue(Constants.PRE_LIVE_STATUS);
    categoryConfigurationHistory.setNewValue(Constants.POST_LIVE_STATUS);
    categoryConfigurationHistory.setActivity(ACTIVITY);
    categoryConfigurationHistory.setCreatedBy(CREATED_BY);
    categoryConfigurationHistory.setCreatedDate(new Date());
    categoryConfigurationHistory.setUpdatedBy(UPDATED_BY);
    categoryConfigurationHistory.setUpdatedDate(new Date());
    categoryConfigurationHistory.setCategoryName(CATEGORY_NAME);

    merchantConfigurationHistory = new MerchantConfigurationHistory();
    merchantConfigurationHistory.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistory.setNewValue(Constants.NEUTRAL_STATUS);
    merchantConfigurationHistory.setActivity(ACTIVITY);
    merchantConfigurationHistory.setOldValue(Constants.PRE_LIVE_FLAG);
    merchantConfigurationHistory.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistory.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationHistory.setCreatedBy(CREATED_BY);
    merchantConfigurationHistory.setCreatedDate(new Date());
    merchantConfigurationHistory.setUpdatedBy(UPDATED_BY);
    merchantConfigurationHistory.setUpdatedDate(new Date());

    product =
      new Product.Builder().productCode(PRODUCT_CODE).name(PRODUCT_NAME).length(ORIGINAL_LENGTH)
        .width(ORIGINAL_WIDTH).height(ORIGINAL_HEIGHT).weight(ORIGINAL_WEIGHT)
        .shippingWeight(ORIGINAL_SHIPPING_WEIGHT).brand(ORIGINAL_BRAND).storeId(STORE_ID).build();
    product.setId(PRODUCT_ID);
    product.setProductItems(new ArrayList<>());
    productItem.setMarkForDelete(true);
    productItemAttributeValue.setProductItem(productItem);
    attribute.setName(BRAND_ATTRIBUTE_NAME);
    productItemAttributeValue.setAttribute(attribute);
    productItemAttributeValue.setValue(ORIGINAL_BRAND);
    productItem.setProductItemAttributeValues(new ArrayList<>());
    productItem.getProductItemAttributeValues().add(productItemAttributeValue);
    product.getProductItems().add(productItem);
    originalBrandAttribute.setPredefinedAllowedAttributeCode(BRAND_ATTRIBUTE_NAME);
    originalBrandAttribute.setValue(ORIGINAL_BRAND);
    updatedBrandAttribute = new PredefinedAllowedAttributeValue();
    updatedBrandAttribute.setPredefinedAllowedAttributeCode(BRAND_ATTRIBUTE_NAME);
    updatedBrandAttribute.setValue(BRAND);
    productAttribute.setProductAttributeName(BRAND_ATTRIBUTE_NAME);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttribute.setAttribute(attribute);
    productAttributeValue.setPredefinedAllowedAttributeValue(originalBrandAttribute);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    product.setProductAttributes(Arrays.asList(productAttribute));
    image.setLocationPath(LOCATION_PATH);
    BeanUtils.copyProperties(image, productItemImage);
    BeanUtils.copyProperties(image, productImage);
    productImage2.setLocationPath(LOCATION_PATH);
    productItemImage2.setLocationPath(LOCATION_PATH);
    productImage3.setLocationPath(LOCATION_PATH1);
    productItemImage3.setLocationPath(LOCATION_PATH1);
    productItemImage.setSequence(10);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage);
    product.setProductImages(productImages);
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    productAndItemImageRequest.setProductImages(Arrays.asList(image));
    productItemImageRequest.setSkuCode(SKU_CODE);
    productItemImageRequest.setItemImages(Arrays.asList(image));
    productAndItemImageRequest.setProductItemImages(Arrays.asList(productItemImageRequest));
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    category.setCatalog(catalog);
    productCategory.setCategory(category);
    productCategory.setProduct(product);
    productCategory1.setCategory(category);
    productCategory1.setProduct(product);
    productCategory1.setMarkForDelete(true);
    product.setProductCategories(Arrays.asList(productCategory, productCategory1));

    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setBasicView(false);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    this.attribute.getAllowedAttributeValues()
      .add(new AllowedAttributeValue(this.attribute, VALUE, VALUE, 1));
    this.attribute.getAllowedAttributeValues()
      .add(new AllowedAttributeValue(this.attribute, VALUE, STORE_ID, 2));
    this.attribute.getAllowedAttributeValues().get(1).setMarkForDelete(false);
    this.attribute.getPredefinedAllowedAttributeValues()
      .add(new PredefinedAllowedAttributeValue(this.attribute, VALUE, STORE_ID, 1));
    this.attribute.getPredefinedAllowedAttributeValues()
      .add(new PredefinedAllowedAttributeValue(this.attribute, VALUE, STORE_ID, 2));
    this.attribute.getPredefinedAllowedAttributeValues().get(1).setMarkForDelete(false);

    productItem.setGeneratedItemName(ITEM_NAME);
    productItem.setSkuCode(SKU_CODE);
    product.setProductCode(PRODUCT_CODE);
    productItem.setProduct(product);

    restrictedKeyword.setKeyword(KEYWORD);
    restrictedKeyword.setId(KEYWORD_ID);

    restrictedKeyword.setValidateOnUi(Boolean.TRUE);
    restrictedKeyword.setValidateByDs(Boolean.FALSE);

    categoryRestrictedKeyword.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeyword.setCategoryId(category.getId());
    categoryRestrictedKeyword.setRestrictedKeywordId(restrictedKeyword.getId());
    categoryRestrictedKeyword.setCategory(category);
    categoryRestrictedKeyword.setRestrictedKeyword(restrictedKeyword);
    categoryRestrictedKeyword.setAction(ACTION);
    categoryRestrictedKeyword.setType(TYPE);
    categoryRestrictedKeyword.setMessage(MESSAGE);
    categoryRestrictedKeyword.setDestinationCategory(DESTINATON_CATEGORY);

    categoryRestrictedKeywordResponse.setCategoryCode(CATEGORY_CODE);
    categoryRestrictedKeywordResponse.setCategoryId(category.getId());
    categoryRestrictedKeywordResponse.setRestrictedKeywordId(restrictedKeyword.getId());
    categoryRestrictedKeywordResponse.setAction(ACTION);
    categoryRestrictedKeywordResponse.setType(TYPE);
    categoryRestrictedKeywordResponse.setMessage(MESSAGE);
    categoryRestrictedKeywordResponse.setDestinationCategory(DESTINATON_CATEGORY);

    categoryKeywordsUpdateDTO.setKeyword(KEYWORD);
    categoryKeywordsUpdateDTO.setKeywordId(KEYWORD_ID);

    categoryKeywordsUpdateDTO2.setKeyword(KEYWORD1);
    categoryKeywordsUpdateDTO2.setKeywordId(KEYWORD_ID1);

    categoryKeywordsUpdateListDTO.setCreatedBy(CREATED_BY);
    categoryKeywordsUpdateListDTO.setUpdatedBy(UPDATED_BY);
    categoryKeywordsUpdateListDTO.setCreatedDate(new Date());
    categoryKeywordsUpdateListDTO.setUpdatedDate(new Date());
    categoryKeywordsUpdateListDTO.setAddedRestrictedKeywords(
      Collections.singletonList(categoryKeywordsUpdateDTO));
    categoryKeywordsUpdateListDTO.setDeletedRestrictedKeywords(
      Collections.singletonList(categoryKeywordsUpdateDTO2));

    keywordMap.put(KEYWORD_ID, restrictedKeyword);

    productSalesCategoryMapping.setOldSalesCategoryCodes(Arrays.asList(OLD_SALES_CATEGORY));
    productSalesCategoryMapping.setNewSalesCategoryCodes(Arrays.asList(NEW_SALES_CATEGORY));
    productSalesCategoryMapping.setOldB2bSalesCategoryCodes(Arrays.asList(OLD_B2B_SALES_CATEGORY));
    productSalesCategoryMapping.setNewB2bSalesCategoryCodes(Arrays.asList(NEW_B2B_SALES_CATEGORY));

    minWholesaleDiscountDTO = new MinWholesaleDiscountDTO();
    minWholesaleDiscountDTO.setPercentage(PERCENTAGE);
    minWholesaleDiscountDTO.setPrice(PRICE);
    wholesaleConfigDTO = new WholesaleConfigDTO();
    wholesaleConfigDTO.setQuantity(QUANTITY);
    wholesaleConfigDTO.setMinWholesaleDiscount(Collections.singletonList(minWholesaleDiscountDTO));
    wholesaleMappingDTO = new WholesaleMappingDTO();
    wholesaleMappingDTO.setConfigurationType(CONFIGURATIONTYPE);
    wholesaleMappingDTO.setWholesaleConfig(Collections.singletonList(wholesaleConfigDTO));

    originalSalesCategory = new OriginalSalesCategory();
    originalSalesCategory.setId(OSC_ID);
    originalSalesCategory.setOscCode(OSC_CODE);
    originalSalesCategory.setOscShortText(OSC_SHORT_TEXT);
    originalSalesCategory.setOscLongText(OSC_LONG_TEXT);

    brandAuthorisation.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisation.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    brandAuthorisation.setStoreId(STORE_ID);
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);

    brandAuthorisationHistory.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationHistory.setActivity(ACTIVITY_AUTH);
    brandAuthorisationHistory.setOldStatus(OLD_VALUE_AUTH);
    brandAuthorisationHistory.setNewStatus(NEW_VALUE_AUTH);

    // Dimension page initialisation
    dimension = new Dimension();
    dimension.setId(ID);
    dimension.setName(VALUE);
    dimensionMapping = new DimensionMapping();
    dimensionMapping.setAttributeCode(ATTRIBUTE_CODE);
    dimensionMapping.setDimension(dimension);
    dimensionMapping.setMandatory(true);
    dimensionMappingList = new ArrayList<>();
    dimensionMappingList.add(dimensionMapping);
    dimensionMappingPage = new PageImpl<>(dimensionMappingList);

    brandAuthHistoryList.add(brandAuthorisationHistory);
    brandAuthHistory = new PageImpl<>(brandAuthHistoryList);
    ConverterUtil.setFileStorageService(fileStorageService);
  }

  @Test
  public void generateSolrAddBrandDomainEventModelForActiveBrand() {
    SolrBrandModel solrBrandModel = ConverterUtil.generateSolrBrandModelApproveExistingBrand(brand);
    assertNotNull(solrBrandModel);
    assertEquals(BRAND_CODE, solrBrandModel.getBrandCode());
  }

  @Test
  public void generateSolrAddBrandDomainEventModelForBrandWip() {
    SolrBrandModel solrBrandModel =
      ConverterUtil.generateSolrBrandModelApproveExistingBrand(brandWip);
    assertNotNull(solrBrandModel);
    assertEquals(BRAND_CODE, solrBrandModel.getBrandCode());
  }

  @Test
  public void generateSolrDeleteBrandDomainEventModelTest() {
    SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel =
      ConverterUtil.generateSolrDeleteBrandDomainEventModel(Arrays.asList(ID));
    assertNotNull(solrDeleteBrandDomainEventModel);
    assertEquals(ID, solrDeleteBrandDomainEventModel.getIds().get(0));
  }

  @Test
  public void generateSolrUpdateBrandDomainEventModelTest() {
    SolrUpdateBrandModel solrUpdateBrandModel =
      ConverterUtil.generateSolrUpdateBrandDomainEventModel(brandWip);
    assertNotNull(solrUpdateBrandModel);
    assertEquals(BRAND_CODE, solrUpdateBrandModel.getBrandCode());
  }

  @Test
  public void generateSolrBrandModelTest() {
    SolrBrandModel solrBrandModel = ConverterUtil.generateSolrBrandModel(brandWip);
    assertNotNull(solrBrandModel);
    assertEquals(BRAND_CODE, solrBrandModel.getBrandCode());
  }

  @Test
  public void generateSolrBrandModelUnDeleteBrandTest() {
    SolrBrandModel solrBrandModel =
      ConverterUtil.generateSolrBrandModelApproveExistingBrand(brandWip);
    assertNotNull(solrBrandModel);
    assertEquals(BRAND_CODE, solrBrandModel.getBrandCode());
  }

  @Test
  public void generateSolrBrandModelForUnDeleteBrandTest() {
    SolrBrandModel solrBrandModel = ConverterUtil.generateSolrBrandModelApproveExistingBrand(brand);
    assertNotNull(solrBrandModel);
    assertEquals(BRAND_CODE, solrBrandModel.getBrandCode());
  }

  @Test
  public void generateBrandWipHistoryResponsesTest() {
    List<BrandWipHistoryResponse> response =
      ConverterUtil.generateBrandWipHistoryResponses(brandWips);
    Assertions.assertEquals(BRAND_CODE, response.get(0).getBrandRequestCode());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, response.get(0).getDescription());
    Assertions.assertEquals(BrandWipState.APPROVED.name(), response.get(0).getState());
  }

  @Test
  public void generateBrandWipResponses() {
    List<BrandWipResponse> responses = ConverterUtil.generateBrandWipResponses(page);
    Assertions.assertEquals(BRAND_CODE, responses.get(0).getBrandCode());
    Assertions.assertEquals(STORE_ID, responses.get(0).getStoreId());
  }

  @Test
  public void generateBrandRejectedInfoResponseTest() {
    brandWip.setNotes(DEFAULT_DESCRIPTION.getBytes());
    BrandRejectionInfoResponse response =
      ConverterUtil.generateBrandRejectionInfoResponse(brandWip);
    Assertions.assertEquals(BRAND_CODE, response.getBrandRequestCode());
  }

  @Test
  public void brandWipToBrandTest() {
    Brand brand = ConverterUtil.brandWipToBrand(brandWip);
    assertEquals(BRAND_CODE, brand.getBrandCode());
    assertEquals(BRAND_LOGO, brand.getBrandLogoPath());
  }

  @Test
  public void generateBrandApprovedOrRejectedDomainEventModelTest() {
    BrandApprovedOrRejectedDomainEventModel response =
      ConverterUtil.generateBrandApprovedOrRejectedDomainEventModel(brandWip);
    assertEquals(BRAND_CODE, response.getBrandCode());
    assertEquals(BrandWipState.DRAFT.name(), response.getBrandApprovalStatus());
    assertEquals(MERCHANT_CODE, response.getBusinessPartnerCode());
  }

  @Test
  public void toCategoryConfigurationFilterResponseListTest() {
    List<CategoryConfigurationFilterResponse> categoryConfigurationFilterResponseList =
      ConverterUtil.toCategoryConfigurationFilterResponseList(Arrays.asList(categoryConfiguration));
    assertNotNull(categoryConfigurationFilterResponseList.get(0).getCreatedDate());
    Assertions.assertEquals(USERNAME, categoryConfigurationFilterResponseList.get(0).getCreatedBy());
    Assertions.assertEquals(CATEGORY_CODE,
      categoryConfigurationFilterResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
      categoryConfigurationFilterResponseList.get(0).getCategoryName());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS,
      categoryConfigurationFilterResponseList.get(0).getReviewConfig());
  }

  @Test
  public void generateCategoryConfigurationHistoryTest() {
    CategoryConfigurationHistory response =
      ConverterUtil.generateCategoryConfigurationHistory(USER_NAME, CATEGORY_CODE, CATEGORY_NAME,
        OLD_VALUE, NEW_VALUE, ACTIVITY, STORE_ID);
    assertEquals(CATEGORY_CODE, response.getCategoryCode());
    assertEquals(OLD_VALUE, response.getOldValue());
    assertEquals(NEW_VALUE, response.getNewValue());
  }

  @Test
  public void generateMerchantConfigurationHistoryTest() {
    MerchantConfigurationHistory response =
      ConverterUtil.generateMerchantConfigurationHistory(USER_NAME, MERCHANT_NAME, MERCHANT_CODE,
        OLD_VALUE, NEW_VALUE, ACTIVITY, STORE_ID);
    assertEquals(MERCHANT_CODE, response.getMerchantCode());
    assertEquals(OLD_VALUE, response.getOldValue());
    assertEquals(NEW_VALUE, response.getNewValue());
  }

  @Test
  public void toMerchantConfigurationFilterResponseListTest() {
    List<MerchantConfigurationFilterResponse> merchantConfigurationFilterResponseList =
      ConverterUtil.toMerchantConfigurationFilterResponseList(Arrays.asList(merchantConfiguration));
    Assertions.assertEquals(MERCHANT_CODE,
      merchantConfigurationFilterResponseList.get(0).getMerchantCode());
    Assertions.assertEquals(MERCHANT_NAME,
      merchantConfigurationFilterResponseList.get(0).getMerchantName());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS,
      merchantConfigurationFilterResponseList.get(0).getReviewConfig());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationFilterResponseList.get(0).getCreatedBy());
    Assertions.assertEquals(CATEGORY_NAME,
      merchantConfigurationFilterResponseList.get(0).getCategoryName());
  }

  @Test
  public void filterProcessedItemImagesOriginalImageNull() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setActive(true);
    boolean status = ConverterUtil.filterProcessedItemImages(productItemImage);
    Assertions.assertEquals(true, status);
  }

  @Test
  public void filterProcessedItemImagesOriginalImageNotNull() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(true);
    productItemImage.setActive(true);
    boolean status = ConverterUtil.filterProcessedItemImages(productItemImage);
    Assertions.assertEquals(false, status);
  }

  @Test
  public void getProductAttributeExtracted() {
    ProductAttributeExtracted productAttributeExtracted =
      ConverterUtil.getProductAttributeExtracted(PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(productAttributeExtracted.getStatus(), ExtractionStatus.PENDING);
    Assertions.assertEquals(productAttributeExtracted.getProductCode(), PRODUCT_CODE);
  }

  @Test
  public void filterProcessedItemImagesEditedNull() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(true);
    productItemImage.setActive(false);
    productItemImage.setEdited(true);
    boolean status = ConverterUtil.filterProcessedItemImages(productItemImage);
    Assertions.assertFalse(status);
  }

  @Test
  public void filterProcessedItemImagesEditedTrueNull() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(true);
    productItemImage.setActive(true);
    productItemImage.setEdited(true);
    boolean status = ConverterUtil.filterProcessedItemImages(productItemImage);
    Assertions.assertTrue(status);
  }

  @Test
  public void filterProcessedImagesOriginalImageNull() {
    ProductImage productImage = new ProductImage();
    productImage.setActive(true);
    boolean status = ConverterUtil.filterProcessedProductImages(productImage);
    Assertions.assertEquals(true, status);
  }

  @Test
  public void filterProcessedImagesEditedNull() {
    ProductImage productImage = new ProductImage();
    productImage.setActive(true);
    productImage.setEdited(true);
    boolean status = ConverterUtil.filterProcessedProductImages(productImage);
    Assertions.assertTrue(status);
  }

  @Test
  public void filterProcessedImagesEditedFalseNull() {
    ProductImage productImage = new ProductImage();
    productImage.setActive(false);
    productImage.setEdited(true);
    boolean status = ConverterUtil.filterProcessedProductImages(productImage);
    Assertions.assertFalse(status);
  }

  @Test
  public void filterProcessedImagesOriginalImageNotNull() {
    ProductImage productImage = new ProductImage();
    productImage.setOriginalImage(true);
    productImage.setActive(true);
    boolean status = ConverterUtil.filterProcessedProductImages(productImage);
    Assertions.assertEquals(false, status);
  }

  @Test
  public void toCategoryConfigurationHistoryResponse() {
    CategoryConfigurationHistoryResponse categoryConfigurationHistoryResponse =
      ConverterUtil.toCategoryConfigurationHistoryResponse(categoryConfigurationHistory);
    Assertions.assertEquals(UPDATED_BY, categoryConfigurationHistoryResponse.getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationHistoryResponse.getCreatedBy());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationHistoryResponse.getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryResponse.getCategoryCode());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS,
      categoryConfigurationHistoryResponse.getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE_STATUS,
      categoryConfigurationHistoryResponse.getNewValue());
    Assertions.assertEquals(ACTIVITY, categoryConfigurationHistoryResponse.getActivity());
    assertNotNull(categoryConfigurationHistoryResponse.getUpdatedDate());
    assertNotNull(categoryConfigurationHistoryResponse.getCreatedDate());
  }

  @Test
  public void toMerchantConfigurationHistoryResponseTest() {
    MerchantConfigurationHistoryResponse merchantConfigurationHistoryResponse =
      ConverterUtil.toMerchantConfigurationHistoryResponse(merchantConfigurationHistory);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
      merchantConfigurationHistoryResponse.getMerchantName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
      merchantConfigurationHistoryResponse.getMerchantCode());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS,
      merchantConfigurationHistoryResponse.getOldValue());
    Assertions.assertEquals(Constants.NEUTRAL_STATUS,
      merchantConfigurationHistoryResponse.getNewValue());
    Assertions.assertEquals(ACTIVITY, merchantConfigurationHistoryResponse.getActivity());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationHistoryResponse.getCreatedBy());
    Assertions.assertEquals(UPDATED_BY, merchantConfigurationHistoryResponse.getUpdatedBy());
    assertNotNull(merchantConfigurationHistoryResponse.getCreatedDate());
    assertNotNull(merchantConfigurationHistoryResponse.getUpdatedDate());
  }

  @Test
  public void toProductItemCompleteResponseTest() {
    ProductItemCompleteResponse productItemCompleteResponse =
      ConverterUtil.toProductItemCompleteResponse(productItem, product);
    Assertions.assertEquals(SKU_CODE, productItemCompleteResponse.getSkuCode());
    Assertions.assertEquals(PRODUCT_NAME, productItemCompleteResponse.getProductResponse().getName());
    Assertions.assertEquals(PRODUCT_CODE,
      productItemCompleteResponse.getProductResponse().getProductCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.toString(),
      productItemCompleteResponse.getProductResponse().getProductAttributeResponses().get(0)
        .getAttribute().getAttributeType());
    Assertions.assertEquals(CATEGORY_CODE,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getName());
  }

  @Test
  public void toProductItemCompleteResponseTest_DescriptiveAttributeValueTypeNone() {
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setAllowedAttributeValue(allowedAttributeValue);
    ProductItemCompleteResponse productItemCompleteResponse =
      ConverterUtil.toProductItemCompleteResponse(productItem, product);
    Assertions.assertEquals(SKU_CODE, productItemCompleteResponse.getSkuCode());
    Assertions.assertEquals(PRODUCT_NAME, productItemCompleteResponse.getProductResponse().getName());
    Assertions.assertEquals(PRODUCT_CODE,
      productItemCompleteResponse.getProductResponse().getProductCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.toString(),
      productItemCompleteResponse.getProductResponse().getProductAttributeResponses().get(0)
        .getAttribute().getAttributeType());
    Assertions.assertEquals(CATEGORY_CODE,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getName());
  }

  @Test
  public void toProductItemCompleteResponseTest_DescriptiveAttributeValueTypeNoneWithMfdProductItemImage() {
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setAllowedAttributeValue(allowedAttributeValue);
    product.getProductItems().stream().map(ProductItem::getProductItemImages).flatMap(List::stream)
      .forEach(productItemImage1 -> productItemImage1.setMarkForDelete(true));
    product.getProductItems().stream().map(ProductItem::getProductItemImages).flatMap(List::stream)
      .forEach(productItemImage1 -> productItemImage1.setOriginalImage(false));
    ProductItemCompleteResponse productItemCompleteResponse =
      ConverterUtil.toProductItemCompleteResponse(productItem, product);
    Assertions.assertEquals(SKU_CODE, productItemCompleteResponse.getSkuCode());
    Assertions.assertEquals(PRODUCT_NAME, productItemCompleteResponse.getProductResponse().getName());
    Assertions.assertEquals(PRODUCT_CODE,
      productItemCompleteResponse.getProductResponse().getProductCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.toString(),
      productItemCompleteResponse.getProductResponse().getProductAttributeResponses().get(0)
        .getAttribute().getAttributeType());
    Assertions.assertEquals(CATEGORY_CODE,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getName());
  }

  @Test
  public void toProductItemCompleteResponseTest_DescriptiveAttributeValueTypeNoneWithOrignalItemImage() {
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setAllowedAttributeValue(allowedAttributeValue);
    product.getProductItems().stream().map(ProductItem::getProductItemImages).flatMap(List::stream)
      .forEach(productItemImage1 -> productItemImage1.setMarkForDelete(false));
    product.getProductItems().stream().map(ProductItem::getProductItemImages).flatMap(List::stream)
      .forEach(productItemImage1 -> productItemImage1.setOriginalImage(false));
    ProductItemCompleteResponse productItemCompleteResponse =
      ConverterUtil.toProductItemCompleteResponse(productItem, product);
    Assertions.assertEquals(SKU_CODE, productItemCompleteResponse.getSkuCode());
    Assertions.assertEquals(PRODUCT_NAME, productItemCompleteResponse.getProductResponse().getName());
    Assertions.assertEquals(PRODUCT_CODE,
      productItemCompleteResponse.getProductResponse().getProductCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.toString(),
      productItemCompleteResponse.getProductResponse().getProductAttributeResponses().get(0)
        .getAttribute().getAttributeType());
    Assertions.assertEquals(CATEGORY_CODE,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getName());
  }

  @Test
  public void toProductItemCompleteResponseTest_OriginalImage2() {
      Product product = new Product();
      ProductItem productItem = new ProductItem();
      productItem.setSkuCode(SKU_CODE);
      ProductItemImage image1 = new ProductItemImage();
      image1.setMarkForDelete(false);
      image1.setOriginalImage(true);
      ProductItemImage image2 = new ProductItemImage();
      image2.setMarkForDelete(true);
      image2.setOriginalImage(true);
      ProductItemImage image3 = new ProductItemImage();
      image3.setMarkForDelete(false);
      image3.setOriginalImage(false);
      productItem.setProductItemImages(new ArrayList<>());
      product.setProductItems(Collections.singletonList(productItem));
      ProductItemCompleteResponse productItemCompleteResponse = ConverterUtil.toProductItemCompleteResponse(productItem, product);
      Assertions.assertEquals(0, productItemCompleteResponse.getImages().size());
    }

  @Test
  public void toProductItemCompleteResponseTest_FilterProductImages() {
    // Create a product with a product item and its images
    Product product = new Product();
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode(SKU_CODE);

    // Create product images with both filter conditions met
    ProductItemImage image1 = new ProductItemImage();
    image1.setMarkForDelete(false);
    image1.setOriginalImage(false);

    ProductItemImage image2 = new ProductItemImage();
    image2.setMarkForDelete(true); // This image should be filtered out
    image2.setOriginalImage(true);

    ProductItemImage image3 = new ProductItemImage();
    image3.setMarkForDelete(false);
    image3.setOriginalImage(true); // This image should be filtered out

    productItem.setProductItemImages(Arrays.asList(image1, image2, image3));
    product.setProductItems(Collections.singletonList(productItem));

    ProductItemCompleteResponse productItemCompleteResponse = ConverterUtil.toProductItemCompleteResponse(productItem, product);

    // Assertions
    // Ensure that the filtered product images do not exist in the response
    Assertions.assertEquals(1, productItemCompleteResponse.getImages().size()); // Only image1 should remain
    Assertions.assertEquals(image1.getId(), productItemCompleteResponse.getImages().get(0).getId());
  }



  @Test
  public void toProductItemCompleteResponseTest_1() {
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setPredefinedAllowedAttributeValue(null);
    product.getProductAttributes().get(0).getProductAttributeValues().get(0)
      .setDescriptiveAttributeValue(DESCRIPTIVE_VALUE);
    ProductItemCompleteResponse productItemCompleteResponse =
      ConverterUtil.toProductItemCompleteResponse(productItem, product);
    Assertions.assertEquals(SKU_CODE, productItemCompleteResponse.getSkuCode());
    Assertions.assertEquals(PRODUCT_NAME, productItemCompleteResponse.getProductResponse().getName());
    Assertions.assertEquals(PRODUCT_CODE,
      productItemCompleteResponse.getProductResponse().getProductCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.toString(),
      productItemCompleteResponse.getProductResponse().getProductAttributeResponses().get(0)
        .getAttribute().getAttributeType());
    Assertions.assertEquals(CATEGORY_CODE,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
      productItemCompleteResponse.getProductResponse().getProductCategoryResponses().get(0)
        .getCategoryResponse().getName());
  }

  @Test
  public void toSimpleItemDetailResponseTest() {
    SimpleItemDetailResponse simpleItemDetailResponse =
      ConverterUtil.toSimpleItemDetailResponse(productItem);
    Assertions.assertEquals(SKU_CODE, simpleItemDetailResponse.getItemCode());
    Assertions.assertEquals(PRODUCT_CODE, simpleItemDetailResponse.getProductCode());
    Assertions.assertEquals(ITEM_NAME, simpleItemDetailResponse.getItemName());
  }

  @Test
  public void toCategoryRestrictedKeywordTest() {
    CategoryRestrictedKeyword result =
      ConverterUtil.toCategoryRestrictedKeyword(STORE_ID, restrictedKeyword, category, new CategoryKeywordsUpdateDTO());
    Assertions.assertEquals(CATEGORY_CODE, result.getCategoryCode());
    Assertions.assertFalse(result.isMarkForDelete());
  }

  @Test
  public void toRestrictedKeywordsTest() {
    Map<String, Boolean> newKeywordsAddedToCategoryValidateDsMap = new HashMap<>();
    List<String> keywords = Arrays.asList(KEYWORD, KEYWORD1);
    List<RestrictedKeyword> result = ConverterUtil.toRestrictedKeywords(keywords, STORE_ID,
      newKeywordsAddedToCategoryValidateDsMap);
    Assertions.assertFalse(result.get(0).isMarkForDelete());
    Assertions.assertEquals(2, result.size());
  }

  @Test
  public void toRestrictedKeywordsResponseFromCategoryRestrictedKeywordTest() {
    categoryRestrictedKeyword.getRestrictedKeyword().setValidateByDs(true);
    RestrictedKeywordsResponse restrictedKeywordsResponse =
      ConverterUtil.toRestrictedKeywordsResponseFromCategoryRestrictedKeyword(
        categoryRestrictedKeyword);
    assertNotNull(restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(KEYWORD, restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(KEYWORD_ID, restrictedKeywordsResponse.getKeywordId());
    Assertions.assertEquals(null, restrictedKeywordsResponse.getSelected());
    Assertions.assertTrue(restrictedKeywordsResponse.getValidateByDs());
  }

  @Test
  public void toRestrictedKeywordsResponseFromCategoryRestrictedKeywordNullTest() {
    categoryRestrictedKeyword.setRestrictedKeyword(null);
    RestrictedKeywordsResponse restrictedKeywordsResponse =
        ConverterUtil.toRestrictedKeywordsResponseFromCategoryRestrictedKeyword(
            categoryRestrictedKeyword);
    Assertions.assertNull(restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(KEYWORD_ID, restrictedKeywordsResponse.getKeywordId());
    Assertions.assertEquals(null, restrictedKeywordsResponse.getSelected());
    Assertions.assertNull(restrictedKeywordsResponse.getValidateByDs());
  }

  @Test
  public void toRestrictedKeywordsResponseFromRestrictedKeywordTest() {
    RestrictedKeywordsResponse restrictedKeywordsResponse =
      ConverterUtil.toRestrictedKeywordsResponseFromRestrictedKeyword(restrictedKeyword);
    assertNotNull(restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(KEYWORD, restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(KEYWORD_ID, restrictedKeywordsResponse.getKeywordId());
    Assertions.assertFalse(restrictedKeywordsResponse.getValidateByDs());
    Assertions.assertEquals(null, restrictedKeywordsResponse.getSelected());
  }

  @Test
  public void toRestrictedKeywordsResponseFromRestrictedKeywordWithValidationFlagTest() {
    RestrictedKeywordsResponse restrictedKeywordsResponse =
      ConverterUtil.toRestrictedKeywordsResponseWithValidationFlagFromRestrictedKeyword(restrictedKeyword);
    assertNotNull(restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(KEYWORD, restrictedKeywordsResponse.getKeyword());
    Assertions.assertEquals(Boolean.FALSE, restrictedKeywordsResponse.getValidateByDs());
    Assertions.assertEquals(Boolean.TRUE, restrictedKeywordsResponse.getValidateOnUi());
  }

  @Test
  public void getProductSalesCategoryMappingResponseTest() {
    ProductSalesCategoryMappingResponse productSalesCategoryMappingResponse =
      ConverterUtil.getProductSalesCategoryMappingResponse(PRODUCT_CODE,
        productSalesCategoryMapping);
    Assertions.assertEquals(productSalesCategoryMappingResponse.getOldSalesCategoryCodes().get(0),
      OLD_SALES_CATEGORY);
    Assertions.assertEquals(productSalesCategoryMappingResponse.getNewSalesCategoryCodes().get(0),
      NEW_SALES_CATEGORY);
    Assertions.assertEquals(productSalesCategoryMappingResponse.getOldB2bSalesCategoryCodes().get(0),
        OLD_B2B_SALES_CATEGORY);
    Assertions.assertEquals(productSalesCategoryMappingResponse.getNewB2bSalesCategoryCodes().get(0),
        NEW_B2B_SALES_CATEGORY);
  }

  @Test
  public void getWholesalePriceConfigurationTest() throws Exception {
    WholesalePriceConfiguration wholeSalePriceConfiguration =
      ConverterUtil.getWholesalePriceConfiguration(STORE_ID, wholesaleMappingDTO, category);
    Assertions.assertEquals(wholeSalePriceConfiguration.getConfigurationType(), CONFIGURATIONTYPE);
    Assertions.assertEquals(wholeSalePriceConfiguration.getWholesaleConfigs(), WHOLESALECONFIG);
    Assertions.assertEquals(category.getId(), wholeSalePriceConfiguration.getCategoryId());
  }

  @Test
  public void getBrandResponseByBrandWipTest() {
    BrandResponse response = ConverterUtil.getBrandResponseByBrandWip(brandWip);
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BRAND_LOGO_PATH, response.getBrandLogoPath());
    Assertions.assertEquals(BRAND, response.getBrandName());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, response.getBrandDescription());
  }

  @Test
  public void getBrandResponseByBrandWipNullBrandCodeTest() {
    brandWip.setBrandCode(null);
    BrandResponse response = ConverterUtil.getBrandResponseByBrandWip(brandWip);
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BRAND_LOGO_PATH, response.getBrandLogoPath());
    Assertions.assertEquals(BRAND, response.getBrandName());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, response.getBrandDescription());
  }

  @Test
  public void getMasterAttributeDomainEventModelTest() {
    attribute.getAllowedAttributeValues().get(1).setValue(VALUE2);
    attribute.getPredefinedAllowedAttributeValues().get(1).setValue(VALUE2);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setCategory(category);
    attribute.setCategoryAttributes(Arrays.asList(categoryAttribute));
    AttributeDomainEventModel attributeDomainEventModel =
      ConverterUtil.getMasterAttributeDomainEventModel(attribute);
    assertEquals(ATTRIBUTE_CODE, attributeDomainEventModel.getAttributeCode());
    assertEquals(AttributeType.DEFINING_ATTRIBUTE.toString(),
      attributeDomainEventModel.getAttributeType());
  }

  @Test
  public void regenerateCopyToAllVariantImagesTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, false);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateCopyToAllVariantImagesCommonImagesTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, false);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateCopyToAllVariantImagesExistingMoreItemsImagesDeletedFilterTest() {
    ProductItem productItem1 = new ProductItem();
    BeanUtils.copyProperties(productItem, productItem1);
    productItem1.setSkuCode("item1");
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemImage productItemImageDeletedImage = new ProductItemImage();
    productItemImageDeletedImage.setLocationPath("/xyz");
    productItemImageDeletedImage.setSequence(1);
    productItemImageDeletedImage.setMarkForDelete(true);
    productItemImageDeletedImage.setMainImages(false);
    productItemImageDeletedImage.setActive(true);
    productItem1.getProductItemImages().add(productItemImageDeletedImage);
    ProductItemImage productItemImageNotActive = new ProductItemImage();
    productItemImageNotActive.setLocationPath("/xyz1");
    productItemImageNotActive.setSequence(1);
    productItemImageNotActive.setMarkForDelete(false);
    productItemImageNotActive.setMainImages(false);
    productItemImageNotActive.setActive(false);
    productItem1.getProductItemImages().add(productItemImageNotActive);
    product.getProductItems().add(productItem1);
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    Image image1 = new Image();
    image1.setLocationPath("/xyz.png");
    image1.setMainImages(false);
    image1.setHashCode("hashCode");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    image1.setMarkForDelete(false);
    copyToAllVariantImages.add(image1);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, false);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(4, product.getProductItems().get(1).getProductItemImages().size());
  }

  @Test
  public void regenerateCopyToAllVariantImagesExistingImageTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, false);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateCopyToAllVariantImagesFalseConditionTest() {
    ProductItem productItem1 = new ProductItem();
    BeanUtils.copyProperties(productItem, productItem1);
    productItem1.setSkuCode("item1");
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath("/xyz");
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(false);
    productItem1.getProductItemImages().add(productItemImage1);
    product.getProductItems().add(productItem1);
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH);
    image1.setMainImages(false);
    image1.setHashCode("hashCode");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    image1.setMarkForDelete(false);
    copyToAllVariantImages.add(image1);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, false);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(2, product.getProductItems().get(1).getProductItemImages().size());
  }




  @Test
  public void regenerateCopyToAllVariantImagesExistingMoreItemsImagesTest() {
    ProductItem productItem1 = new ProductItem();
    BeanUtils.copyProperties(productItem, productItem1);
    productItem1.setSkuCode("item1");
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath("/xyz");
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(false);
    productItem1.getProductItemImages().add(productItemImage1);
    product.getProductItems().add(productItem1);
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    Image image1 = new Image();
    image1.setLocationPath("/xyz.png");
    image1.setMainImages(false);
    image1.setHashCode("hashCode");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    image1.setMarkForDelete(false);
    copyToAllVariantImages.add(image1);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, false);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(3, product.getProductItems().get(1).getProductItemImages().size());
  }

  @Test
  public void regenerateAddNewProductItemImageTest() {
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH2);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    newProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateAddNewProductItemImage(product, newProductItemImages,
        productPublishUpdateDTO, false, false, false);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateAddNewProductItemImageSameLocationPathTest() {
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    newProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateAddNewProductItemImage(product, newProductItemImages, productPublishUpdateDTO, false, false, false);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateAddNewProductItemImageWrongSkuCodeTest() {
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode("sku-code1");
    newProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateAddNewProductItemImage(product, newProductItemImages, productPublishUpdateDTO, false, false, false);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMainImages());
  }

  @Test
  public void regenerateAddNewProductItemImageLocationPathTest() {
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH2);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    newProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateAddNewProductItemImage(product, newProductItemImages,
        productPublishUpdateDTO, true, true, false);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateAddNewProductItemImageLocationPathMultipleActiveMainImagesTest() {
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH2);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(true);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    newProductItemImages.add(productItemImageRequest);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    ConverterUtil.regenerateAddNewProductItemImage(product, newProductItemImages,
        productPublishUpdateDTO, true, false, true);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(1, product.getProductItems().get(0)
        .getProductItemImages().stream().filter(ProductItemImage::isMainImages).count());
  }

  @Test
  public void regenerateAddNewProductItemImageLocationPathMultipleMainImagesTest() {
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH2);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    newProductItemImages.add(productItemImageRequest);
    product.getProductItems().get(0).getProductItemImages().get(0).setMainImages(true);
    ConverterUtil.regenerateAddNewProductItemImage(product, newProductItemImages,
        productPublishUpdateDTO, true, true, true);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(1, product.getProductItems().get(0)
        .getProductItemImages().stream().filter(ProductItemImage::isMainImages).count());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedCommonImageTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setCommonImage(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertTrue(
        product.getProductItems().get(0).getProductItemImages().get(0).isMainImages());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedDeleteImageTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedWrongSkuCodeTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode("sku-code1");
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateProductItemImageNoUpdatedTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH + "1");
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedDeleteResizeImageTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedDeleteResizeImage2Test() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage.setSequence(1);
    productItemImage.setMarkForDelete(false);
    productItemImage.setMainImages(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH1);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
  }

  @Test
  public void regenerateProductItemImageGotUpdatedDeleteResizeImage1Test() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(RESIZE_LOCATION_PATH + ".jpg");
    productItemImage.setSequence(1);
    productItemImage.setMarkForDelete(false);
    productItemImage.setMainImages(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH1);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductItemImageGotUpdated(product, updatedProductItemImages, productPublishUpdateDTO);
    Assertions.assertEquals(3, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesUpdateTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(RESIZE_LOCATION_PATH + ".jpg");
    productItemImage.setSequence(1);
    productItemImage.setMarkForDelete(false);
    productItemImage.setMainImages(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(RESIZE_LOCATION_PATH.replace("resize/", ""));
    productItemImage2.setSequence(1);
    productItemImage2.setMarkForDelete(false);
    productItemImage2.setOriginalImage(true);
    productItemImage2.setMainImages(false);
    productItemImage2.setActive(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage2);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH1);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(false);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, false, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertFalse(product.getProductImages().get(0).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesUpdate1Test() {
    List<ProductItemImageRequest> updatedProductItemImages = getProductItemImageRequests();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(true);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, false, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertTrue(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(2).isMarkForDelete());
    Assertions.assertFalse(product.getProductImages().get(3).isMarkForDelete());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(3).isMarkForDelete());
  }


  @Test
  public void regenerateProductImagesUpdate_trueTest() {
    List<ProductItemImageRequest> updatedProductItemImages = getProductItemImageRequests();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(false);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, true, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertTrue(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(3).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesUpdate_falseTest() {
    List<ProductItemImageRequest> updatedProductItemImages = getProductItemImageRequests();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setMarkForDelete(true);
    product.getProductImages().get(0).setOriginalImage(true);
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(false);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, true, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertTrue(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
    Assertions.assertTrue(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(3).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesUpdatefalseCommonImageTrueTest() {
    List<ProductItemImageRequest> updatedProductItemImages = getProductItemImageRequests();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setMarkForDelete(true);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductImages().get(1).setCommonImage(true);
    product.getProductItems().get(0).getProductItemImages().get(1).setCommonImage(true);
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(false);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, true, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertTrue(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
    Assertions.assertTrue(
        product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(3).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesUpdatefalseActiveTrueTest() {
    List<ProductItemImageRequest> updatedProductItemImages = getProductItemImageRequests();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setMarkForDelete(true);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().get(0).getProductItemImages().get(2).setActive(true);
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(false);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, true, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertTrue(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
    Assertions.assertTrue(
        product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(3).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesUpdateTest1() {
    List<ProductItemImageRequest> updatedProductItemImages = getProductItemImageRequests();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setMarkForDelete(false);
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setOriginalImage(null);
    product.getProductItems().get(0).getProductItemImages().get(0).setMarkForDelete(false);
    product.getProductItems().get(0).getProductItemImages().get(0).setActive(true);
    product.getProductItems().get(0).getProductItemImages().get(0).setOriginalImage(null);
    Mockito.when(fileStorageService.isFinalImageFileExist(Mockito.anyString())).thenReturn(true);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, true, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertFalse(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertFalse(product.getProductImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(0).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(1).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(2).isMarkForDelete());
    Assertions.assertFalse(
      product.getProductItems().get(0).getProductItemImages().get(3).isMarkForDelete());
  }

  private List<ProductItemImageRequest> getProductItemImageRequests() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    productItemImageRequest.setItemImages(new ArrayList<>());
    product.getProductItems().get(0).getProductItemImages().clear();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(
      "/87/MTA-0615254/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    productItemImage.setSequence(1);
    productItemImage.setMarkForDelete(true);
    productItemImage.setMainImages(true);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(
      "/MTA-0615254/resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(true);
    productItemImage1.setActive(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(
      "MTA-0615254/nike_test_product_and_item_images_full01_hhp7330d.jpeg");
    productItemImage2.setSequence(1);
    productItemImage2.setMarkForDelete(false);
    productItemImage2.setOriginalImage(true);
    productItemImage2.setMainImages(true);
    productItemImage2.setActive(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage2);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath("MTA-0615254/nike_test_product_and_item_images_full02.jpeg");
    productItemImage3.setSequence(1);
    productItemImage3.setMarkForDelete(false);
    productItemImage3.setOriginalImage(true);
    productItemImage3.setMainImages(true);
    productItemImage3.setActive(false);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage3);
    product.getProductImages().clear();

    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(
      "/87/MTA-0615254/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    productImage.setSequence(1);
    productImage.setMarkForDelete(true);
    productImage.setMainImages(true);
    productImage.setActive(true);
    product.getProductImages().add(productImage);
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath(
      "/MTA-0615254/resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    productImage1.setSequence(1);
    productImage1.setMarkForDelete(false);
    productImage1.setMainImages(true);
    productImage1.setActive(false);
    product.getProductImages().add(productImage1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(
      "MTA-0615254/nike_test_product_and_item_images_full01_hhp7330d.jpeg");
    productImage2.setSequence(1);
    productImage2.setMarkForDelete(false);
    productImage2.setOriginalImage(true);
    productImage2.setMainImages(true);
    productImage2.setActive(false);
    product.getProductImages().add(productImage2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath("MTA-0615254/nike_test_product_and_item_images_full02.jpeg");
    productImage3.setSequence(1);
    productImage3.setMarkForDelete(false);
    productImage3.setOriginalImage(true);
    productImage3.setMainImages(true);
    productImage3.setActive(false);
    product.getProductImages().add(productImage3);

    Image image = new Image();
    image.setLocationPath("/87/MTA-0615254/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    image.setSequence(1);
    image.setMarkForDelete(true);
    image.setMainImages(true);
    image.setActive(true);
    productItemImageRequest.getItemImages().add(image);
    Image image1 = new Image();
    image1.setLocationPath(
      "/MTA-0615254/resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    image1.setSequence(1);
    image1.setMarkForDelete(false);
    image1.setMainImages(true);
    image1.setActive(false);
    productItemImageRequest.getItemImages().add(image1);
    Image image2 = new Image();
    image2.setLocationPath("MTA-0615254/nike_test_product_and_item_images_full01_hhp7330d.jpeg");
    image2.setSequence(1);
    image2.setMarkForDelete(false);
    image2.setOriginalImage(true);
    image2.setMainImages(true);
    image2.setActive(false);
    productItemImageRequest.getItemImages().add(image2);

    updatedProductItemImages.add(productItemImageRequest);
    return updatedProductItemImages;
  }

  @Test
  public void regenerateProductResizeImagesUpdateTest() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(RESIZE_LOCATION_PATH + ".jpg");
    productItemImage.setSequence(1);
    productItemImage.setMarkForDelete(false);
    productItemImage.setMainImages(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(true);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ProductImage imageResize = new ProductImage();
    imageResize.setLocationPath(RESIZE_LOCATION_PATH);
    imageResize.setMainImages(true);
    imageResize.setHashCode("hashCode");
    imageResize.setSequence(1);
    imageResize.setOriginalImage(true);
    imageResize.setActive(false);
    imageResize.setMarkForDelete(false);
    product.getProductImages().add(imageResize);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, false, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertTrue(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
  }

  @Test
  public void regenerateProductResizeImagesUpdate1Test() {
    List<ProductItemImageRequest> updatedProductItemImages = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(RESIZE_LOCATION_PATH + ".jpg");
    productItemImage.setSequence(1);
    productItemImage.setMarkForDelete(false);
    productItemImage.setMainImages(false);
    productItemImage.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(RESIZE_LOCATION_PATH);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    product.getProductItems().get(0).getProductItemImages().add(productItemImage1);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(true);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(image);
    productItemImageRequest.setSkuCode(SKU_CODE);
    updatedProductItemImages.add(productItemImageRequest);
    ProductImage imageResize = new ProductImage();
    imageResize.setLocationPath(RESIZE_LOCATION_PATH1);
    imageResize.setMainImages(true);
    imageResize.setHashCode("hashCode");
    imageResize.setSequence(1);
    imageResize.setOriginalImage(true);
    imageResize.setActive(false);
    imageResize.setMarkForDelete(false);
    product.getProductImages().add(imageResize);
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath(RESIZE_LOCATION_PATH);
    productImage1.setMainImages(true);
    productImage1.setHashCode("hashCode");
    productImage1.setSequence(1);
    productImage1.setOriginalImage(true);
    productImage1.setActive(false);
    productImage1.setMarkForDelete(false);
    product.getProductImages().add(productImage1);
    ConverterUtil.regenerateProductImagesUpdate(product, updatedProductItemImages, false, productPublishUpdateDTO, new HashSet<>());
    Assertions.assertFalse(product.getProductImages().get(0).isMarkForDelete());
    Assertions.assertFalse(product.getProductImages().get(1).isMarkForDelete());
  }

  @Test
  public void regenerateProductImagesTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateProductImages(product, copyToAllVariantImages, new ArrayList<>(),
      false, new HashSet<>());
    Assertions.assertEquals(1, product.getProductImages().size());
  }

  @Test
  public void convertToBrandSummaryResponseTest() {
    Brand brand = generateBrand();
    List<BrandSummaryResponse> brandSummaryResponses =
      ConverterUtil.convertToBrandSummaryResponse(Arrays.asList(brand));
    Assertions.assertEquals(DEFAULT_BRAND_NAME, brandSummaryResponses.get(0).getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandSummaryResponses.get(0).getBrandCode());
    Assertions.assertEquals(BRAND_WIP_ID, brandSummaryResponses.get(0).getBrandWipId());
    Assertions.assertTrue(brandSummaryResponses.get(0).isValidBrand());
  }

  private Brand generateBrand() {
    Brand brand = new Brand();
    brand.setId(ID);
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    brand.setBrandName(DEFAULT_BRAND_NAME);
    brand.setBrandWipId(BRAND_WIP_ID);
    brand.setValidBrand(true);
    return brand;
  }

  @Test
  public void regenerateCopyToAllVariantImagesNeedCorrectionTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, true);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertTrue(product.getProductItems().get(0).getProductItemImages().get(1).isRevised());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isEdited());
  }

  @Test
  public void regenerateCopyToAllVariantImagesCommonImagesNeedCorrectionTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, true);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(0).isRevised());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(0).isEdited());
  }

  @Test
  public void regenerateCopyToAllVariantImagesExistingMoreItemsImagesDeletedFilterNeedCorrectionTest() {
    ProductItem productItem1 = new ProductItem();
    BeanUtils.copyProperties(productItem, productItem1);
    productItem1.setSkuCode("item1");
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemImage productItemImageDeletedImage = new ProductItemImage();
    productItemImageDeletedImage.setLocationPath("/xyz");
    productItemImageDeletedImage.setSequence(1);
    productItemImageDeletedImage.setMarkForDelete(true);
    productItemImageDeletedImage.setMainImages(false);
    productItemImageDeletedImage.setActive(true);
    productItem1.getProductItemImages().add(productItemImageDeletedImage);
    ProductItemImage productItemImageNotActive = new ProductItemImage();
    productItemImageNotActive.setLocationPath("/xyz1");
    productItemImageNotActive.setSequence(1);
    productItemImageNotActive.setMarkForDelete(false);
    productItemImageNotActive.setMainImages(false);
    productItemImageNotActive.setActive(false);
    productItem1.getProductItemImages().add(productItemImageNotActive);
    product.getProductItems().add(productItem1);
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    Image image1 = new Image();
    image1.setLocationPath("/xyz.png");
    image1.setMainImages(false);
    image1.setHashCode("hashCode");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    image1.setMarkForDelete(false);
    image1.setEdited(false);
    image1.setRevised(true);
    copyToAllVariantImages.add(image1);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, true);
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(4, product.getProductItems().get(1).getProductItemImages().size());
    Assertions.assertTrue(product.getProductItems().get(0).getProductItemImages().get(1).isRevised());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isEdited());
    Assertions.assertTrue(product.getProductItems().get(1).getProductItemImages().get(2).isRevised());
    Assertions.assertFalse(product.getProductItems().get(1).getProductItemImages().get(2).isEdited());
  }

  @Test
  public void regenerateCopyToAllVariantImagesExistingImageNeedCorrectionTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, true);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(0).isRevised());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(0).isEdited());
  }

  @Test
  public void regenerateCopyToAllVariantImagesExistingMoreItemsImageNeedCorrectionTest() {
    ProductItem productItem1 = new ProductItem();
    BeanUtils.copyProperties(productItem, productItem1);
    productItem1.setSkuCode("item1");
    productItem1.setProductItemImages(new ArrayList<>());
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH);
    productItemImage1.setSequence(1);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setMainImages(false);
    productItemImage1.setActive(true);
    productItemImage1.setRevised(true);
    productItem1.getProductItemImages().add(productItemImage1);
    product.getProductItems().add(productItem1);
    product.getProductItems().get(0).getProductItemImages().get(0).setRevised(true);
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(false);
    image.setHashCode("hashCode");
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    ConverterUtil.regenerateCopyToAllVariantImages(product, copyToAllVariantImages, true);
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
    Assertions.assertEquals(1, product.getProductItems().get(1).getProductItemImages().size());
  }

  @Test
  public void regenerateProductImagesNeedCorrectionTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    Image image1 = new Image();
    image1.setLocationPath("/xyz1.png");
    image1.setMainImages(false);
    image1.setHashCode("hash-code");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    image1.setMarkForDelete(false);
    image1.setEdited(false);
    image1.setRevised(true);
    copyToAllVariantImages.add(image1);

    Image productImage = new Image();
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setMainImages(true);
    productImage.setHashCode("hashCode");
    productImage.setSequence(1);
    productImage.setOriginalImage(true);
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setRevised(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(productImage);
    productItemImageRequest.setSkuCode(SKU_CODE);
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    newProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductImages(product, copyToAllVariantImages, newProductItemImages,
      true, new HashSet<>());
    Assertions.assertEquals(2, product.getProductImages().size());
    Assertions.assertTrue(product.getProductImages().get(1).isRevised());
  }

  @Test
  public void regenerateProductImagesNeedCorrectionCommonImageTrueTest() {
    List<Image> copyToAllVariantImages = new ArrayList<>();
    Image image = new Image();
    image.setLocationPath("/xyz.png");
    image.setMainImages(false);
    image.setHashCode(null);
    image.setSequence(1);
    image.setOriginalImage(true);
    image.setActive(false);
    image.setMarkForDelete(false);
    image.setEdited(false);
    image.setRevised(true);
    copyToAllVariantImages.add(image);
    Image image1 = new Image();
    image1.setLocationPath("/xyz1.png");
    image1.setMainImages(false);
    image1.setHashCode("hash-code");
    image1.setSequence(1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    image1.setMarkForDelete(false);
    image1.setEdited(false);
    image1.setRevised(true);
    image1.setCommonImage(true);
    copyToAllVariantImages.add(image1);

    Image productImage = new Image();
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setMainImages(true);
    productImage.setHashCode("hashCode");
    productImage.setSequence(1);
    productImage.setOriginalImage(true);
    productImage.setActive(false);
    productImage.setMarkForDelete(true);
    productImage.setRevised(true);
    productItemImageRequest.setItemImages(new ArrayList<>());
    productItemImageRequest.getItemImages().add(productImage);
    productItemImageRequest.setSkuCode(SKU_CODE);
    List<ProductItemImageRequest> newProductItemImages = new ArrayList<>();
    newProductItemImages.add(productItemImageRequest);
    ConverterUtil.regenerateProductImages(product, copyToAllVariantImages, newProductItemImages,
        true, new HashSet<>());
    Assertions.assertEquals(2, product.getProductImages().size());
    Assertions.assertTrue(product.getProductImages().get(1).isRevised());
  }

  @Test
  public void imageNameToListOfImageMapTest() {
    Map<String, List<ProductImage>> response = ConverterUtil.imageNameToListOfImageMap(
      Arrays.asList(productImage, productImage2, productImage3));
    assertNotNull(response);
    Assertions.assertEquals(LOCATION_PATH, response.get(IMAGE_NAME).get(0).getLocationPath());
    Assertions.assertEquals(LOCATION_PATH1, response.get(IMAGE_NAME1).get(0).getLocationPath());
  }

  @Test
  public void imageNameToListOfItemImageMapTest() {
    Map<String, List<ProductItemImage>> response = ConverterUtil.imageNameToListOfItemImageMap(
      Arrays.asList(productItemImage, productItemImage2, productItemImage3));
    assertNotNull(response);
    Assertions.assertEquals(LOCATION_PATH, response.get(IMAGE_NAME).get(0).getLocationPath());
    Assertions.assertEquals(LOCATION_PATH1, response.get(IMAGE_NAME1).get(0).getLocationPath());
  }

  @Test
  public void toOriginalSalesCategoryResponseTest() {
    OriginalSalesCategoryResponse response =
      ConverterUtil.toOriginalSalesCategoryResponse(originalSalesCategory);
    assertNotNull(response);
    Assertions.assertEquals(OSC_CODE, response.getOscCode());
    Assertions.assertEquals(OSC_SHORT_TEXT, response.getOscShortText());
    Assertions.assertEquals(OSC_LONG_TEXT, response.getOscLongText());
    Assertions.assertEquals(0, response.getMasterCategories().size());
  }

  @Test
  public void toOriginalSalesCategoryResponse_WithMasterCategoriesTest() {
    originalSalesCategory.setMasterCategories(Arrays.asList(category));
    OriginalSalesCategoryResponse response =
      ConverterUtil.toOriginalSalesCategoryResponse(originalSalesCategory);
    assertNotNull(response);
    Assertions.assertEquals(OSC_CODE, response.getOscCode());
    Assertions.assertEquals(OSC_SHORT_TEXT, response.getOscShortText());
    Assertions.assertEquals(OSC_LONG_TEXT, response.getOscLongText());
    Assertions.assertEquals(UPDATED_BY, response.getMasterCategories().get(0).getOscUpdatedBy());
    assertNotNull(response.getMasterCategories().get(0).getOscUpdatedDate());
    Assertions.assertEquals(1, response.getMasterCategories().size());
  }

  @Test
  public void toMatrixAttributeExtractionRequestTest() {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setName(PRODUCT_NAME);
    product.setBrand(BRAND_NAME);
    product.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    product.setDescription(DESCRIPTION.getBytes());

    ProductAttributeExtractionModel productAttributeExtractionModel =
      new ProductAttributeExtractionModel();
    productAttributeExtractionModel.setCnCategoryCode(CATEGORY_CODE);

    MatrixAttributeExtractionRequest matrixAttributeExtractionRequest =
      ConverterUtil.toMatrixAttributeExtractionRequest(product, productAttributeExtractionModel);

    Assertions.assertEquals(PRODUCT_CODE, matrixAttributeExtractionRequest.getProduct_code());
    Assertions.assertEquals(PRODUCT_NAME, matrixAttributeExtractionRequest.getTitle());
    Assertions.assertEquals(BRAND_NAME, matrixAttributeExtractionRequest.getBrand());
    Assertions.assertEquals(UNIQUE_SELLING_POINT, matrixAttributeExtractionRequest.getProduct_usp());
    Assertions.assertEquals(DESCRIPTION, matrixAttributeExtractionRequest.getDescription());
    Assertions.assertEquals(CATEGORY_CODE, matrixAttributeExtractionRequest.getCn_code());
  }

  @Test
  public void toProductMasterDataResponseTest() {
    ProductItemUomInfo productItemUomInfo = new ProductItemUomInfo();
    productItem.setProductItemUomInfo(productItemUomInfo);
    productItemImage2.setMarkForDelete(true);
    productItem.getProductItemImages().add(productItemImage2);
    productImage2.setMarkForDelete(true);
    product.getProductImages().add(productImage2);
    product.getProductCategories().get(1).setCategory(new Category());
    product.getProductCategories().get(1).getCategory().setCategoryCode(CATEGORY_CODE_1);
    product.setCreatedMerchant(DEFAULT_SELLER_CODE);
    ProductMasterDataResponse productMasterDataResponse =
      ConverterUtil.toProductMasterDataResponse(product, productItem);
    Assertions.assertEquals(PRODUCT_CODE, productMasterDataResponse.getProductCode());
    Assertions.assertEquals(LOCATION_PATH,
      productMasterDataResponse.getImages().get(0).getLocationPath());
    Assertions.assertEquals(CATEGORY_CODE,
      productMasterDataResponse.getProductCategoryResponses().get(0).getCategory()
        .getCategoryCode());
    Assertions.assertEquals(SKU_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getSkuCode());
    Assertions.assertEquals(LOCATION_PATH,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getImages().get(0)
        .getLocationPath());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0)
        .getProductItemAttributeValueResponses().get(0).getAttributeResponse().getAttributeCode());
    Assertions.assertEquals(1, productMasterDataResponse.getProductCategoryResponses().size());
    Assertions.assertEquals(DEFAULT_SELLER_CODE, productMasterDataResponse.getSellerCode());
  }

  @Test
  public void toProductMasterDataResponse_reverseCategoryOrderTest() {
    productItemImage2.setMarkForDelete(true);
    productItem.getProductItemImages().add(productItemImage2);
    productImage2.setMarkForDelete(true);
    product.getProductImages().add(productImage2);
    product.getProductCategories().get(1).setCategory(new Category());
    product.getProductCategories().get(1).getCategory().setCategoryCode(CATEGORY_CODE_1);
    Collections.reverse(product.getProductCategories());
    ProductMasterDataResponse productMasterDataResponse =
      ConverterUtil.toProductMasterDataResponse(product, productItem);
    Assertions.assertEquals(PRODUCT_CODE, productMasterDataResponse.getProductCode());
    Assertions.assertEquals(LOCATION_PATH,
      productMasterDataResponse.getImages().get(0).getLocationPath());
    Assertions.assertEquals(CATEGORY_CODE,
      productMasterDataResponse.getProductCategoryResponses().get(0).getCategory()
        .getCategoryCode());
    Assertions.assertEquals(SKU_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getSkuCode());
    Assertions.assertEquals(LOCATION_PATH,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getImages().get(0)
        .getLocationPath());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0)
        .getProductItemAttributeValueResponses().get(0).getAttributeResponse().getAttributeCode());
    Assertions.assertEquals(1, productMasterDataResponse.getProductCategoryResponses().size());
  }

  @Test
  public void toProductMasterDataResponse_multipleActiveCategoryTest() {
    productItemImage2.setMarkForDelete(true);
    productItem.getProductItemImages().add(productItemImage2);
    productImage2.setMarkForDelete(true);
    product.getProductImages().add(productImage2);
    product.getProductCategories().get(1).setCategory(new Category());
    product.getProductCategories().get(1).getCategory().setCategoryCode(CATEGORY_CODE_1);
    product.getProductCategories().get(1).setMarkForDelete(false);
    ProductMasterDataResponse productMasterDataResponse =
      ConverterUtil.toProductMasterDataResponse(product, productItem);
    Assertions.assertEquals(PRODUCT_CODE, productMasterDataResponse.getProductCode());
    Assertions.assertEquals(LOCATION_PATH,
      productMasterDataResponse.getImages().get(0).getLocationPath());
    Assertions.assertEquals(CATEGORY_CODE,
      productMasterDataResponse.getProductCategoryResponses().get(0).getCategory()
        .getCategoryCode());
    Assertions.assertEquals(SKU_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getSkuCode());
    Assertions.assertEquals(LOCATION_PATH,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getImages().get(0)
        .getLocationPath());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0)
        .getProductItemAttributeValueResponses().get(0).getAttributeResponse().getAttributeCode());
    Assertions.assertEquals(2, productMasterDataResponse.getProductCategoryResponses().size());
  }

  @Test
  public void toProductMasterDataResponse_multipleInActiveCategoryTest() {
    productItemImage2.setMarkForDelete(true);
    productItem.getProductItemImages().add(productItemImage2);
    productImage2.setMarkForDelete(true);
    product.getProductImages().add(productImage2);
    product.getProductCategories().get(1).setCategory(new Category());
    product.getProductCategories().get(1).getCategory().setCategoryCode(CATEGORY_CODE_1);
    product.getProductCategories().get(0).setMarkForDelete(true);
    ProductMasterDataResponse productMasterDataResponse =
      ConverterUtil.toProductMasterDataResponse(product, productItem);
    Assertions.assertEquals(PRODUCT_CODE, productMasterDataResponse.getProductCode());
    Assertions.assertEquals(LOCATION_PATH,
      productMasterDataResponse.getImages().get(0).getLocationPath());
    Assertions.assertEquals(CATEGORY_CODE,
      productMasterDataResponse.getProductCategoryResponses().get(0).getCategory()
        .getCategoryCode());
    Assertions.assertEquals(SKU_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getSkuCode());
    Assertions.assertEquals(LOCATION_PATH,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getImages().get(0)
        .getLocationPath());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0)
        .getProductItemAttributeValueResponses().get(0).getAttributeResponse().getAttributeCode());
    Assertions.assertEquals(2, productMasterDataResponse.getProductCategoryResponses().size());
  }

  @Test
  public void toProductMasterDataResponseCatalogNullTest() throws JsonProcessingException {
    product.getProductCategories().get(0).getCategory().setCatalog(null);
    ProductItemUomInfo productItemUomInfo = new ProductItemUomInfo();
    List<DimensionsAndUomResponse> dimensionsAndUomResponseList = new ArrayList<>();
    DimensionsAndUomResponse dimensionsAndUomResponse = new DimensionsAndUomResponse();
    dimensionsAndUomResponseList.add(dimensionsAndUomResponse);
    ObjectMapper objectMapper = new ObjectMapper();
    productItemUomInfo.setUom(objectMapper.writeValueAsString(dimensionsAndUomResponseList));
    productItem.setProductItemUomInfo(productItemUomInfo);
    ProductMasterDataResponse productMasterDataResponse =
      ConverterUtil.toProductMasterDataResponse(product, productItem);
    Assertions.assertEquals(PRODUCT_CODE, productMasterDataResponse.getProductCode());
    Assertions.assertEquals(LOCATION_PATH,
      productMasterDataResponse.getImages().get(0).getLocationPath());
    Assertions.assertEquals(CATEGORY_CODE,
      productMasterDataResponse.getProductCategoryResponses().get(0).getCategory()
        .getCategoryCode());
    Assertions.assertEquals(SKU_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getSkuCode());
    Assertions.assertEquals(LOCATION_PATH,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getImages().get(0)
        .getLocationPath());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0)
        .getProductItemAttributeValueResponses().get(0).getAttributeResponse().getAttributeCode());
  }

  @Test
  public void toProductMasterDataResponseCatalogTypeNullTest() {
    product.getProductCategories().get(0).getCategory().getCatalog().setCatalogType(null);
    ProductMasterDataResponse productMasterDataResponse =
      ConverterUtil.toProductMasterDataResponse(product, productItem);
    Assertions.assertEquals(PRODUCT_CODE, productMasterDataResponse.getProductCode());
    Assertions.assertEquals(LOCATION_PATH,
      productMasterDataResponse.getImages().get(0).getLocationPath());
    Assertions.assertEquals(CATEGORY_CODE,
      productMasterDataResponse.getProductCategoryResponses().get(0).getCategory()
        .getCategoryCode());
    Assertions.assertEquals(SKU_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getSkuCode());
    Assertions.assertEquals(LOCATION_PATH,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0).getImages().get(0)
        .getLocationPath());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0)
        .getProductItemAttributeValueResponses().get(0).getAttributeResponse().getAttributeCode());
  }


  @Test
  public void updateItemImagesDeleteTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMarkForDelete(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(0, errorMap.size());
  }

  @Test
  public void updateItemImagesDeleteMainImageTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMarkForDelete(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.CANNOT_DELETE_MAIN_IMAGE.getMessage()));
  }

  @Test
  public void updateItemImagesDeleteMainImageFalseTest() {
    Map<String, String> errorMap = new HashMap<>();
    Set<String> mainImageAdded = new HashSet<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMarkForDelete(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    String skuCode = product.getProductItems().get(0).getSkuCode();
    mainImageAdded.add(skuCode);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, mainImageAdded, false);
  }

  @Test
  public void updateItemImagesDeleteMainImageFalseAndcontainsCommonMainImageTrueTest() {
    Map<String, String> errorMap = new HashMap<>();
    Set<String> mainImageAdded = new HashSet<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMarkForDelete(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    String skuCode = product.getProductItems().get(0).getSkuCode();
    mainImageAdded.add(skuCode);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
        LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, mainImageAdded, true);
  }

  @Test
  public void updateItemImagesDeleteMainImageFalsecontainsCommonMainImageTest() {
    Map<String, String> errorMap = new HashMap<>();
    Set<String> mainImageAdded = new HashSet<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMarkForDelete(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    String skuCode = "SKU";
    mainImageAdded.add(skuCode);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
        LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, mainImageAdded, true);
  }

  @Test
  public void updateItemImagesDeleteImageNotPresentTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMarkForDelete(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.IMAGE_NOT_PRESENT.getMessage()));
  }

  @Test
  public void updateItemImagesCopyImageTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(true);
    productItemImage.setSequence(1);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(0, errorMap.size());
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateItemImagesCopyImageAlreadyPresentTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.IMAGE_ALREADY_EXISTS.getMessage()));
  }

  @Test
  public void updateItemImagesCopyImageMaxAlreadyReachedTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setCopy(true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(LOCATION_PATH2);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(LOCATION_PATH3);
    ProductItemImage productItemImage4 = new ProductItemImage();
    productItemImage4.setLocationPath(LOCATION_PATH4);
    ProductItemImage productItemImage5 = new ProductItemImage();
    productItemImage5.setLocationPath(LOCATION_PATH5);
    ProductItemImage productItemImage6 = new ProductItemImage();
    productItemImage6.setLocationPath(LOCATION_PATH6);
    ProductItemImage productItemImage7 = new ProductItemImage();
    productItemImage7.setLocationPath(LOCATION_PATH7);
    ProductItemImage productItemImage8 = new ProductItemImage();
    productItemImage8.setLocationPath(LOCATION_PATH8);
    ProductItemImage productItemImage9 = new ProductItemImage();
    productItemImage9.setLocationPath(LOCATION_PATH9);
    ProductItemImage productItemImage10 = new ProductItemImage();
    productItemImage10.setLocationPath(LOCATION_PATH10);
    ProductItemImage productItemImage11 = new ProductItemImage();
    productItemImage11.setLocationPath(LOCATION_PATH11);
    ProductItemImage productItemImage12 = new ProductItemImage();
    productItemImage12.setLocationPath(LOCATION_PATH12);
    ProductItemImage productItemImage13 = new ProductItemImage();
    productItemImage13.setLocationPath(LOCATION_PATH13);
    ProductItemImage productItemImage14 = new ProductItemImage();
    productItemImage14.setLocationPath(LOCATION_PATH14);
    ProductItemImage productItemImage15 = new ProductItemImage();
    productItemImage15.setLocationPath(LOCATION_PATH15);
    ProductItemImage productItemImage16 = new ProductItemImage();
    productItemImage16.setLocationPath(LOCATION_PATH16);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(
      Arrays.asList(productItemImage1, productItemImage2, productItemImage3, productItemImage4,
        productItemImage5, productItemImage6, productItemImage7, productItemImage8,
        productItemImage9, productItemImage10, productItemImage11, productItemImage12,
        productItemImage13, productItemImage14, productItemImage15, productItemImage16));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.MAX_IMAGE_REACHED.getMessage()));
  }

  @Test
  public void updateItemImagesAddImageTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(true);
    productItemImage.setSequence(1);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(0, errorMap.size());
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }


  @Test
  public void updateItemImagesAddImageNeedRevisionTrueTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(true);
    productItemImage.setSequence(1);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, true, true, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(0, errorMap.size());
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateItemImagesAddImageAlreadyPresentTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.IMAGE_ALREADY_EXISTS.getMessage()));
  }

  @Test
  public void updateItemImagesAddImageMaxAlreadyReachedTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(LOCATION_PATH2);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(LOCATION_PATH3);
    ProductItemImage productItemImage4 = new ProductItemImage();
    productItemImage4.setLocationPath(LOCATION_PATH4);
    ProductItemImage productItemImage5 = new ProductItemImage();
    productItemImage5.setLocationPath(LOCATION_PATH5);
    ProductItemImage productItemImage6 = new ProductItemImage();
    productItemImage6.setLocationPath(LOCATION_PATH6);
    ProductItemImage productItemImage7 = new ProductItemImage();
    productItemImage7.setLocationPath(LOCATION_PATH7);
    ProductItemImage productItemImage8 = new ProductItemImage();
    productItemImage8.setLocationPath(LOCATION_PATH8);
    ProductItemImage productItemImage9 = new ProductItemImage();
    productItemImage9.setLocationPath(LOCATION_PATH9);
    ProductItemImage productItemImage10 = new ProductItemImage();
    productItemImage10.setLocationPath(LOCATION_PATH10);
    ProductItemImage productItemImage11 = new ProductItemImage();
    productItemImage11.setLocationPath(LOCATION_PATH11);
    ProductItemImage productItemImage12 = new ProductItemImage();
    productItemImage12.setLocationPath(LOCATION_PATH12);
    ProductItemImage productItemImage13 = new ProductItemImage();
    productItemImage13.setLocationPath(LOCATION_PATH13);
    ProductItemImage productItemImage14 = new ProductItemImage();
    productItemImage14.setLocationPath(LOCATION_PATH14);
    ProductItemImage productItemImage15 = new ProductItemImage();
    productItemImage15.setLocationPath(LOCATION_PATH15);
    ProductItemImage productItemImage16 = new ProductItemImage();
    productItemImage16.setLocationPath(LOCATION_PATH16);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(
      Arrays.asList(productItemImage1, productItemImage2, productItemImage3, productItemImage4,
        productItemImage5, productItemImage6, productItemImage7, productItemImage8,
        productItemImage9, productItemImage10, productItemImage11, productItemImage12,
        productItemImage13, productItemImage14, productItemImage15, productItemImage16));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, true, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.MAX_IMAGE_REACHED.getMessage()));
  }

  @Test
  public void updateItemImagesMainImageTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMainImage(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(false);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0)
      .setProductItemImages(Arrays.asList(productItemImage, productItemImage1));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(0, errorMap.size());
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateItemImagesMainImageTrueTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMainImage(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0)
      .setProductItemImages(Arrays.asList(productItemImage, productItemImage1));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(0, errorMap.size());
    Assertions.assertEquals(2, product.getProductItems().get(0).getProductItemImages().size());
  }

  @Test
  public void updateItemImagesMainImageNotPresentTest() {
    Map<String, String> errorMap = new HashMap<>();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setMainImage(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setLocationPath(LOCATION_PATH1);
    productItemImage.setMainImages(false);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ConverterUtil.updateItemImages(product.getProductItems().get(0), copyImageEditRequest,
      LOCATION_PATH, errorMap, false, false, productPublishUpdateDTO, new HashSet<>(), false);
    Assertions.assertEquals(1, errorMap.size());
    Assertions.assertTrue(errorMap.containsKey(product.getProductItems().get(0).getSkuCode()));
    Assertions.assertTrue(errorMap.get(product.getProductItems().get(0).getSkuCode())
      .contains(ErrorMessage.IMAGE_NOT_PRESENT.getMessage()));
  }

  @Test
  public void generateBrandResponseTest() {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    brand.setBrandLogoPath("brandlogo/path");
    brand.setProfileBannerPath("brandlogo/path");
    BrandResponse brandResponse = ConverterUtil.generateBrandResponse(brand);
    assertNotNull(brandResponse);
  }

  @Test
  public void generateBrandResponseNullTest() {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    BrandResponse brandResponse = ConverterUtil.generateBrandResponse(brand);
    assertNotNull(brandResponse);
  }

  @Test
  public void generateBrandResponseDescNonNullTest() {
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    brand.setBrandDescription(BRAND_CODE.getBytes());
    BrandResponse brandResponse = ConverterUtil.generateBrandResponse(brand);
    assertNotNull(brandResponse);
  }

  @Test
  public void generateBrandNullTest() {
    BrandResponse brandResponse = ConverterUtil.generateBrandResponse(null);
    Assertions.assertNull(brandResponse);
  }

  @Test
  public void toAttributeValueResponseTest() {
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setValue(VALUE);
    attribute.setAllowedAttributeValues(Arrays.asList(allowedAttributeValue));

    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(VALUE2);
    attribute.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValue));

    AttributeResponse attributeResponse = ConverterUtil.toAttributeValueResponse(attribute);

    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE.name(), attributeResponse.getAttributeType());
    Assertions.assertEquals(VALUE, attributeResponse.getAllowedAttributeValues().get(0).getValue());
    Assertions.assertEquals(VALUE2, attributeResponse.getPredefinedAllowedAttributeValues().get(0).getValue());

  }

  @Test
  public void convertProductItemToItemImageResponseTest() {
    List<ItemImageResponse> itemImageResponses =
      ConverterUtil.convertProductItemToItemImageResponse(Collections.singletonList(productItem), false);
    Assertions.assertEquals(productItem.getSkuCode(), itemImageResponses.get(0).getItemCode());
    Assertions.assertEquals(1, itemImageResponses.get(0).getImageResponses().size());
  }

  @Test
  public void convertProductItemToItemImageResponseOriginalImageFalseTest() {
    productItem.getProductItemImages().get(0).setOriginalImage(true);
    List<ItemImageResponse> itemImageResponses =
        ConverterUtil.convertProductItemToItemImageResponse(Collections.singletonList(productItem), true);
    Assertions.assertEquals(productItem.getSkuCode(), itemImageResponses.get(0).getItemCode());
    Assertions.assertEquals(0, itemImageResponses.get(0).getImageResponses().size());
  }

  @Test
  public void convertProductImageToImageResponseTest(){
    List<ImageResponse> imageResponses =
      ConverterUtil.convertProductImageToImageResponse(Collections.singletonList(productImage), false);
    Assertions.assertEquals(productImage.getLocationPath(), imageResponses.get(0).getLocationPath());
    Assertions.assertEquals(1, imageResponses.size());
  }

  @Test
  public void convertProductImageToImageResponseOriginalImageFalseTest(){
    productImage.setOriginalImage(true);
    List<ImageResponse> imageResponses =
        ConverterUtil.convertProductImageToImageResponse(Collections.singletonList(productImage), true);
    Assertions.assertEquals(0, imageResponses.size());
  }


  @Test
  public void convertToBrandAuthDetailResponseTest() {
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
    Assertions.assertEquals(DEFAULT_SELLER_CODE, brandAuthorisationDetailResponse.getSellerId());
  }

  @Test
  public void convertToBrandAuthDetailResponse_withDateValidationTest() {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(2))));
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
    Assertions.assertEquals(DEFAULT_SELLER_CODE, brandAuthorisationDetailResponse.getSellerId());
    Assertions.assertEquals(BrandAuthorisationStatus.ACTIVE.name(),
      brandAuthorisationDetailResponse.getAuthorisationStatus().name());
  }

  @Test
  public void convertToBrandAuthDetailResponse_withDateValidationInactiveTest() {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(2))));
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
    Assertions.assertEquals(DEFAULT_SELLER_CODE, brandAuthorisationDetailResponse.getSellerId());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(),
      brandAuthorisationDetailResponse.getAuthorisationStatus().name());
  }

  @Test
  public void convertToBrandAuthDetailResponse_withDateValidationExpiredTest() {
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(10))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
    Assertions.assertEquals(DEFAULT_SELLER_CODE, brandAuthorisationDetailResponse.getSellerId());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(),
      brandAuthorisationDetailResponse.getAuthorisationStatus().name());
  }

  @Test
  public void convertToBrandAuthDetailResponse_withDateInValidTest() {
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(11))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
    Assertions.assertEquals(DEFAULT_SELLER_CODE, brandAuthorisationDetailResponse.getSellerId());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(),
      brandAuthorisationDetailResponse.getAuthorisationStatus().name());
  }

  @Test
  public void convertToBrandAuthDetailResponse_withDateStatusValidationTest() {
    brandAuthorisation.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthorisation.setAuthExpireDate(Date.from(Instant.now().minus(Duration.ofDays(10))));
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse =
      ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
    Assertions.assertEquals(DEFAULT_SELLER_CODE, brandAuthorisationDetailResponse.getSellerId());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(),
      brandAuthorisationDetailResponse.getAuthorisationStatus().name());
  }

  @Test
  public void toRestrictedKeywordHistoryResponseTest() {
    RestrictedKeywordHistory restrictedKeywordHistory = new RestrictedKeywordHistory();
    restrictedKeywordHistory.setActivity(ACTIVITY);
    restrictedKeywordHistory.setKeywordId(KEYWORD_ID);
    restrictedKeywordHistory.setOldValue(OLD_VALUE);
    restrictedKeywordHistory.setNewValue(NEW_VALUE);
    restrictedKeywordHistory.setCreatedBy(CREATED_BY);
    RestrictedKeywordHistoryResponse restrictedKeywordHistoryResponse =
        ConverterUtil.toRestrictedKeywordHistoryResponse(restrictedKeywordHistory);
    Assertions.assertEquals(ACTIVITY, restrictedKeywordHistoryResponse.getActivity());
    Assertions.assertEquals(KEYWORD_ID, restrictedKeywordHistoryResponse.getKeywordId());
    Assertions.assertEquals(OLD_VALUE, restrictedKeywordHistoryResponse.getOldValue());
    Assertions.assertEquals(NEW_VALUE, restrictedKeywordHistoryResponse.getNewValue());
    Assertions.assertEquals(CREATED_BY, restrictedKeywordHistoryResponse.getCreatedBy());
  }

  @Test
  public void toRestrictedKeywordsListingResponseTest() {
    RestrictedKeywordsListingResponse response = ConverterUtil.toRestrictedKeywordsListingResponse(restrictedKeyword);
    Assertions.assertEquals(KEYWORD, response.getKeyword());
    Assertions.assertEquals(KEYWORD_ID, response.getKeywordId());
    Assertions.assertEquals(true, response.getValidateOnUi());
  }

  @Test
  public void toBrandAuthFilterResponseTest() {
    BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    brandAuthorisation.setBrandName(BRAND_NAME);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    List<BrandAuthFilterResponse> responses = ConverterUtil.toBrandAuthFilterResponse(
        new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthorisation))),
        BrandAuthorisationStatus.INACTIVE.name(), 1);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void toBrandAuthFilterResponseDocumentLinksTest() {
    BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    brandAuthorisation.setBrandName(BRAND_NAME);
    brandAuthorisation.setDocumentLink(DOCUMENT_LINKS);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    List<BrandAuthFilterResponse> responses = ConverterUtil.toBrandAuthFilterResponse(
        new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthorisation))),
        BrandAuthorisationStatus.INACTIVE.name(), 1);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void toBrandAuthFilterResponseNearExpiryFilterSelected_responseExpired() {
    BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setBrandName(BRAND_NAME);
    brandAuthorisation.setDocumentLink(DOCUMENT_LINKS);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Date yesterdayDate = calendar.getTime();
    brandAuthorisation.setAuthStartDate(yesterdayDate);
    brandAuthorisation.setAuthExpireDate(yesterdayDate);
    List<BrandAuthFilterResponse> responses = ConverterUtil.toBrandAuthFilterResponse(
        new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthorisation))),
        BrandAuthorisationStatus.NEAR_EXPIRY.name(), 1);
    Assertions.assertEquals(1, responses.size());
    Assertions.assertEquals(BrandAuthorizationWipStatus.EXPIRED.name(),
        responses.get(0).getStatus());
  }

  @Test
  public void toBrandAuthFilterResponseNearExpiryFilterSelected_responseNearExpiry() {
    BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setBrandName(BRAND_NAME);
    brandAuthorisation.setDocumentLink(DOCUMENT_LINKS);
    brandAuthorisation.setAuthStartDate(new Date());
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date authExpireDate = calendar.getTime();
    brandAuthorisation.setAuthExpireDate(authExpireDate);
    List<BrandAuthFilterResponse> responses = ConverterUtil.toBrandAuthFilterResponse(
        new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthorisation))),
        BrandAuthorisationStatus.NEAR_EXPIRY.name(), 1);
    Assertions.assertEquals(1, responses.size());
    Assertions.assertEquals(BrandAuthorizationWipStatus.NEAR_EXPIRY.name(),
        responses.get(0).getStatus());
  }

  @Test
  public void toBrandAuthFilterResponseNearExpiryThresholdNotBreached() {
    BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisation.setBrandName(BRAND_NAME);
    brandAuthorisation.setDocumentLink(DOCUMENT_LINKS);
    brandAuthorisation.setAuthStartDate(new Date());
    brandAuthorisation.setAuthExpireDate(new Date());
    List<BrandAuthFilterResponse> responses = ConverterUtil.toBrandAuthFilterResponse(
        new PageImpl<>(new ArrayList<>(Arrays.asList(brandAuthorisation))),
        BrandAuthorisationStatus.ACTIVE.name(), -1);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void getPredictionIdAndCategoryCodeResponsesTest() {
    Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap = new HashMap<>();
    List<CategoryCodeAndNameResponse> categoryCodeAndNameResponseList = new ArrayList<>();
    CategoryCodeAndNameResponse categoryCodeAndNameResponse = new CategoryCodeAndNameResponse();
    categoryCodeAndNameResponse.setCategoryCode(CATEGORY_CODE);
    categoryCodeAndNameResponse.setCategoryName(CATEGORY_NAME);
    categoryCodeAndNameResponseList.add(categoryCodeAndNameResponse);
    CategoryCodeAndNameResponse categoryCodeAndNameResponse1 = new CategoryCodeAndNameResponse();
    categoryCodeAndNameResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryCodeAndNameResponse1.setCategoryName(CATEGORY_NAME_1);
    categoryCodeAndNameResponseList.add(categoryCodeAndNameResponse1);
    predictionIdAndCategoryCodesMap.put(PREDICTION_ID, categoryCodeAndNameResponseList);

    List<CategoryCodeAndNameResponse> categoryCodeAndNameResponseList1 = new ArrayList<>();
    CategoryCodeAndNameResponse categoryCodeAndNameResponse2 = new CategoryCodeAndNameResponse();
    categoryCodeAndNameResponse2.setCategoryCode(CATEGORY_CODE);
    categoryCodeAndNameResponse2.setCategoryName(CATEGORY_NAME);
    categoryCodeAndNameResponseList1.add(categoryCodeAndNameResponse2);
    predictionIdAndCategoryCodesMap.put(PREDICTION_ID_1, categoryCodeAndNameResponseList1);

    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponses =
        ConverterUtil.getPredictionIdAndCategoryCodeResponses(predictionIdAndCategoryCodesMap);

    Assertions.assertEquals(PREDICTION_ID_1, predictionIdAndCategoryCodeResponses.get(0).getPredictionId());
    Assertions.assertEquals(CATEGORY_NAME,
        predictionIdAndCategoryCodeResponses.get(0).getCategoryCodeAndNameResponseList().get(0).getCategoryName());
    Assertions.assertEquals(PREDICTION_ID, predictionIdAndCategoryCodeResponses.get(1).getPredictionId());
    Assertions.assertEquals(CATEGORY_NAME_1,
        predictionIdAndCategoryCodeResponses.get(1).getCategoryCodeAndNameResponseList().get(1).getCategoryName());
  }

  @Test
  public void getPredictionIdAndCategoryCodesMapTest() {
    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingList.add(predictionCategoryMapping);
    PredictionCategoryMapping predictionCategoryMapping1 = new PredictionCategoryMapping();
    predictionCategoryMapping1.setPredictionId(PREDICTION_ID_1);
    predictionCategoryMapping1.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingList.add(predictionCategoryMapping1);

    Map<String, String> categoryCodeAndNameMap = new HashMap<>();
    categoryCodeAndNameMap.put(CATEGORY_CODE, CATEGORY_NAME);
    categoryCodeAndNameMap.put(CATEGORY_CODE_1, CATEGORY_NAME_1);

    Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap =
        ConverterUtil.getPredictionIdAndCategoryCodesMap(predictionCategoryMappingList, categoryCodeAndNameMap);

    Assertions.assertEquals(CATEGORY_NAME_1, predictionIdAndCategoryCodesMap.get(PREDICTION_ID_1).get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE_1, predictionIdAndCategoryCodesMap.get(PREDICTION_ID_1).get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, predictionIdAndCategoryCodesMap.get(PREDICTION_ID).get(0).getCategoryName());
  }

  @Test
  public void getPredictionIdAndCategoryCodesMapPredictionIdPresentCategoryNameEmptyTest() {
    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingList.add(predictionCategoryMapping);
    PredictionCategoryMapping predictionCategoryMapping1 = new PredictionCategoryMapping();
    predictionCategoryMapping1.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping1.setCategoryCode(StringUtils.EMPTY);
    predictionCategoryMappingList.add(predictionCategoryMapping1);

    Map<String, String> categoryCodeAndNameMap = new HashMap<>();
    categoryCodeAndNameMap.put(CATEGORY_CODE, CATEGORY_NAME);
    categoryCodeAndNameMap.put(CATEGORY_CODE_1, CATEGORY_NAME_1);

    Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap =
        ConverterUtil.getPredictionIdAndCategoryCodesMap(predictionCategoryMappingList, categoryCodeAndNameMap);

    Assertions.assertEquals(CATEGORY_NAME_1, predictionIdAndCategoryCodesMap.get(PREDICTION_ID).get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE_1, predictionIdAndCategoryCodesMap.get(PREDICTION_ID).get(0).getCategoryCode());
  }

  @Test
  public void getPredictionIdAndCategoryCodesMapCategoryNameEmptyTest() {
    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    PredictionCategoryMapping predictionCategoryMapping = new PredictionCategoryMapping();
    predictionCategoryMapping.setPredictionId(PREDICTION_ID);
    predictionCategoryMapping.setCategoryCode(CATEGORY_CODE);
    predictionCategoryMappingList.add(predictionCategoryMapping);
    PredictionCategoryMapping predictionCategoryMapping1 = new PredictionCategoryMapping();
    predictionCategoryMapping1.setPredictionId(PREDICTION_ID_1);
    predictionCategoryMapping1.setCategoryCode(CATEGORY_CODE_1);
    predictionCategoryMappingList.add(predictionCategoryMapping1);

    Map<String, String> categoryCodeAndNameMap = new HashMap<>();

    Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap =
        ConverterUtil.getPredictionIdAndCategoryCodesMap(predictionCategoryMappingList, categoryCodeAndNameMap);

    Assertions.assertTrue(predictionIdAndCategoryCodesMap.isEmpty());
  }

  @Test
  public void generateBrandAuthHistoryResponsesTest() {
    brandAuthHistory.getContent().forEach(history ->
    history.setUpdatedBy(UPDATED_BY));
    List<BrandAuthHistoryResponse> response =
      ConverterUtil.generateBrandAuthHistoryResponses(brandAuthHistory,
        BrandAuthHistoryRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
          .build());
    Assertions.assertEquals(BRAND_CODE, response.get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_SELLER_CODE, response.get(0).getSellerCode());
    Assertions.assertEquals(OLD_VALUE_AUTH, response.get(0).getOldStatus());
    Assertions.assertEquals(NEW_VALUE_AUTH, response.get(0).getNewStatus());
    Assertions.assertEquals(UPDATED_BY, response.get(0).getUpdatedBy());
  }

  @Test
  public void generateBrandAuthHistoryResponsesWithCreatedByTest() {
    brandAuthHistory.getContent().stream().forEach(history -> {
      history.setCreatedBy(CREATED_BY);
      history.setUpdatedBy(null);
    });
    List<BrandAuthHistoryResponse> response =
      ConverterUtil.generateBrandAuthHistoryResponses(brandAuthHistory,
        BrandAuthHistoryRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
          .build());
    Assertions.assertEquals(BRAND_CODE, response.get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_SELLER_CODE, response.get(0).getSellerCode());
    Assertions.assertEquals(OLD_VALUE_AUTH, response.get(0).getOldStatus());
    Assertions.assertEquals(NEW_VALUE_AUTH, response.get(0).getNewStatus());
    Assertions.assertEquals(CREATED_BY, response.get(0).getUpdatedBy());
  }

  @Test
  public void generateBrandAuthHistoryResponsesWithUsernameTest() {
    brandAuthHistory.getContent().stream().forEach(history -> {
      history.setCreatedBy(CREATED_BY);
      history.setUpdatedBy(UPDATED_BY);
    });
    List<BrandAuthHistoryResponse> response =
      ConverterUtil.generateBrandAuthHistoryResponses(brandAuthHistory,
        BrandAuthHistoryRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
          .userName(UPDATED_BY).build());
    Assertions.assertEquals(BRAND_CODE, response.get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_SELLER_CODE, response.get(0).getSellerCode());
    Assertions.assertEquals(OLD_VALUE_AUTH, response.get(0).getOldStatus());
    Assertions.assertEquals(NEW_VALUE_AUTH, response.get(0).getNewStatus());
    Assertions.assertEquals(UPDATED_BY, response.get(0).getUpdatedBy());
  }

  @Test
  public void generateBrandAuthHistoryResponsesWithNoUsernameTestTest() {
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthorisationHistory.setActivity(ACTIVITY_AUTH);
    brandAuthorisationHistory.setOldStatus(OLD_VALUE_AUTH);
    brandAuthorisationHistory.setNewStatus(NEW_VALUE_AUTH);
    brandAuthorisationHistory.setUpdatedBy(USERNAME);
    BrandAuthorisationHistory brandAuthorisationHistory2 = new BrandAuthorisationHistory();
    brandAuthorisationHistory2.setBrandCode(BRAND_CODE);
    brandAuthorisationHistory2.setSellerCode(DEFAULT_SELLER_CODE);
    Page<BrandAuthorisationHistory> brandAuthHistory = new PageImpl<>(Arrays.asList(brandAuthorisationHistory,
      brandAuthorisationHistory2));
    List<BrandAuthHistoryResponse> response =
      ConverterUtil.generateBrandAuthHistoryResponses(brandAuthHistory,
        BrandAuthHistoryRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE).userName(USERNAME).build());
    Assertions.assertEquals(USERNAME, response.get(0).getUpdatedBy());
    Assertions.assertEquals(1,response.size());
  }

  @Test
  public void convertToBrandAuthBulkResponseTest() {
    List<BrandAuthBulkDownloadResponse> responseList =
      ConverterUtil.convertToBrandAuthBulkResponse(Arrays.asList(brandAuthorisation));
    assertNotNull(responseList);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, responseList.get(0).getBrandCode());
  }

  @Test
  public void toBrandAuthHistoryEventModel(){
    BrandAuthorisation finalData = new BrandAuthorisation();
    finalData.setAuthorisationStatus(BrandAuthorisationStatus.INACTIVE);
    finalData.setSellerCode(DEFAULT_SELLER_CODE);
    finalData.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    BrandAuthorisationHistory authorisationHistory =
      ConverterUtil.toBrandAuthHistoryEventModel(finalData, brandAuthorisation);
    Assertions.assertEquals(authorisationHistory.getOldStatus(),
      brandAuthorisation.getAuthorisationStatus().name());
    Assertions.assertEquals(authorisationHistory.getNewStatus(),
      finalData.getAuthorisationStatus().name());
  }

  @Test
  public void isCategoryRestrictedKeywordDataUpdatedNoUpdateTest() {
    boolean categoryRestrictedKeywordDataUpdated = ConverterUtil
        .isCategoryRestrictedKeywordDataUpdated(new CategoryKeywordsUpdateDTO(), new CategoryRestrictedKeyword());
    Assertions.assertFalse(categoryRestrictedKeywordDataUpdated);
  }

  @Test
  public void isCategoryRestrictedKeywordDataUpdatedTypeChangeTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setType(LOCATION_PATH);
    boolean categoryRestrictedKeywordDataUpdated = ConverterUtil
        .isCategoryRestrictedKeywordDataUpdated(categoryKeywordsUpdateDTO, new CategoryRestrictedKeyword());
    Assertions.assertTrue(categoryRestrictedKeywordDataUpdated);
  }

  @Test
  public void isCategoryRestrictedKeywordDataUpdatedMessageChangeTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setMessage(LOCATION_PATH);
    boolean categoryRestrictedKeywordDataUpdated = ConverterUtil
        .isCategoryRestrictedKeywordDataUpdated(categoryKeywordsUpdateDTO, new CategoryRestrictedKeyword());
    Assertions.assertTrue(categoryRestrictedKeywordDataUpdated);
  }

  @Test
  public void isCategoryRestrictedKeywordDataUpdatedActionChangeTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setAction(10);
    boolean categoryRestrictedKeywordDataUpdated = ConverterUtil
        .isCategoryRestrictedKeywordDataUpdated(categoryKeywordsUpdateDTO, new CategoryRestrictedKeyword());
    Assertions.assertTrue(categoryRestrictedKeywordDataUpdated);
  }

  @Test
  public void isCategoryRestrictedKeywordDataUpdatedCategoryChangeTest() {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    categoryKeywordsUpdateDTO.setDestinationCategory(LOCATION_PATH);
    boolean categoryRestrictedKeywordDataUpdated = ConverterUtil
        .isCategoryRestrictedKeywordDataUpdated(categoryKeywordsUpdateDTO, new CategoryRestrictedKeyword());
    Assertions.assertTrue(categoryRestrictedKeywordDataUpdated);
  }

  @Test
  public void isCategoryRestrictedKeywordDataUpdatedMarkForDeleteChangeTest() {
    CategoryRestrictedKeyword categoryRestrictedKeyword = new CategoryRestrictedKeyword();
    categoryRestrictedKeyword.setMarkForDelete(true);
    boolean categoryRestrictedKeywordDataUpdated = ConverterUtil
        .isCategoryRestrictedKeywordDataUpdated(new CategoryKeywordsUpdateDTO(), categoryRestrictedKeyword);
    Assertions.assertTrue(categoryRestrictedKeywordDataUpdated);
  }

  @Test
  public void toCategoryRestrictedKeywordResponseTest() {
    categoryRestrictedKeywordResponse.setValidateByDs(null);
    CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponseCheck =
        ConverterUtil.toCategoryRestrictedKeywordResponse(categoryRestrictedKeyword);
    Assertions.assertEquals(categoryRestrictedKeywordResponseCheck, categoryRestrictedKeywordResponse);
  }

  @Test
  public void toCategoryRestrictedKeywordResponseTest1() {
    categoryRestrictedKeyword.setRestrictedKeyword(null);
    CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponseCheck = ConverterUtil
        .toCategoryRestrictedKeywordResponse(categoryRestrictedKeyword);
    Assertions.assertEquals(categoryRestrictedKeywordResponseCheck,categoryRestrictedKeywordResponse);
  }

  @Test
  public void toCategoryRestrictedKeywordWithKeywordResponseTest() {
    categoryRestrictedKeywordResponse.setKeyword(categoryRestrictedKeyword.getRestrictedKeyword().getKeyword());
    categoryRestrictedKeywordResponse.setValidateByDs(
        categoryRestrictedKeyword.getRestrictedKeyword().getValidateByDs());
    CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponseCheck =
        ConverterUtil.toCategoryRestrictedKeywordResponseWithKeyword(categoryRestrictedKeyword);
    Assertions.assertEquals(categoryRestrictedKeywordResponseCheck, categoryRestrictedKeywordResponse);
    Assertions.assertEquals(categoryRestrictedKeyword.getRestrictedKeyword().getKeyword(),
        categoryRestrictedKeywordResponseCheck.getKeyword());
    Assertions.assertEquals(categoryRestrictedKeyword.getRestrictedKeyword().getValidateByDs(),
        categoryRestrictedKeywordResponseCheck.getValidateByDs());
  }

  @Test
  public void toCategoryRestrictedKeywordResponseWithKeywordTest1() {
    categoryRestrictedKeyword.setRestrictedKeyword(null);
    CategoryRestrictedKeywordResponse categoryRestrictedKeywordResponseCheck = ConverterUtil
        .toCategoryRestrictedKeywordResponseWithKeyword(categoryRestrictedKeyword);
    Assertions.assertEquals(categoryRestrictedKeywordResponseCheck,categoryRestrictedKeywordResponse);
  }

  @Test
  public void validateDimensionTest() {
    boolean response1 = ConverterUtil.validateDimension(null, null, null, null);
    Assertions.assertTrue(response1);
    boolean response2 = ConverterUtil.validateDimension(1.0, 1.0, 1.0, null);
    Assertions.assertTrue(response2);
    boolean response3 = ConverterUtil.validateDimension(1.0, 1.0, 1.0, 1.0);
    Assertions.assertFalse(response3);
    boolean response4 = ConverterUtil.validateDimension(0.0, 0.0, 0.0, 0.0);
    Assertions.assertTrue(response4);
    boolean response5 = ConverterUtil.validateDimension(1.0, 1.0, 1.0, 0.0);
    Assertions.assertTrue(response5);
  }

  @Test
  public void toListOfRestrictedKeywordsForUiValidationTest() {
    List<RestrictedKeyword> restrictedKeywordList = new ArrayList<>();
    RestrictedKeyword restrictedKeywords = new RestrictedKeyword();
    RestrictedKeyword restrictedKeyword1 = new RestrictedKeyword();
    restrictedKeywords.setKeyword(KEYWORD);
    restrictedKeywords.setId(KEYWORD_ID);
    restrictedKeyword1.setKeyword(KEYWORD1);
    restrictedKeyword1.setId(KEYWORD_ID1);
    restrictedKeywordList.add(restrictedKeywords);
    restrictedKeywordList.add(restrictedKeyword1);
    List<UiValidationRestrictedKeywordsResponse> response =
        ConverterUtil.toListOfRestrictedKeywordsForUiValidation(restrictedKeywordList);
    Assertions.assertEquals(KEYWORD, response.get(0).getKeyword());
    Assertions.assertEquals(KEYWORD_ID, response.get(0).getKeywordId());
    Assertions.assertEquals(KEYWORD1, response.get(1).getKeyword());
    Assertions.assertEquals(KEYWORD_ID1, response.get(1).getKeywordId());
  }

  @Test
  public void toListOfRestrictedKeywordsForUiValidationEmptyListTest() {
    List<RestrictedKeyword> restrictedKeywordList = new ArrayList<>();
    List<UiValidationRestrictedKeywordsResponse> response =
        ConverterUtil.toListOfRestrictedKeywordsForUiValidation(restrictedKeywordList);
    Assertions.assertEquals(new ArrayList<>(), response);
  }

  @Test
  public void testConvertToProduct() {
    ProductDTO productDTO = new ProductDTO();
    List<ProductCategory> productCategoryDTOs = new ArrayList<>();
    List<ProductAttribute> productAttribute = new ArrayList<>();
    List<ProductImage> productImageDTOs = new ArrayList<>();
    List<ProductItem> productItemDTOs = new ArrayList<>();
    productDTO.setProductCategories(productCategoryDTOs);
    productDTO.setProductAttributes(productAttribute);
    productDTO.setProductImages(productImageDTOs);
    productDTO.setProductItems(productItemDTOs);
    Product product = ConverterUtil.convertProductDTOToProduct(productDTO);
    assertEquals(productDTO.getProductCategories(), product.getProductCategories());
    assertEquals(productDTO.getProductAttributes(), product.getProductAttributes());
    assertEquals(productDTO.getProductImages(), product.getProductImages());
    assertEquals(productDTO.getProductItems(), product.getProductItems());
  }

  @Test
  public void testConvertToProductWithCategories() {
    ProductDTO productDTO = new ProductDTO();
    Category category1 = new Category();
    category1.setProductCategories(Collections.singletonList(productCategory));
    category1.setCategoryCode(CATEGORY_CODE);
    category1.setCategoryAttributes(Collections.singletonList(new CategoryAttribute()));
   ProductCategory productCategoryDTO =
     new ProductCategory();
    List<ProductCategory> productCategoryDTOs = Collections.singletonList(productCategoryDTO);
    List<ProductAttribute> productAttributeDTOs = new ArrayList<>();
    List<ProductImage> productImageDTOs = new ArrayList<>();
    List<ProductItem> productItemDTOs = new ArrayList<>();
    productDTO.setProductCategories(productCategoryDTOs);
    productDTO.setProductAttributes(productAttributeDTOs);
    productDTO.setProductImages(productImageDTOs);
    productDTO.setProductItems(productItemDTOs);
    Product product = ConverterUtil.convertProductDTOToProduct(productDTO);
    assertEquals(productDTO.getProductAttributes(), product.getProductAttributes());
    assertEquals(productDTO.getProductImages(), product.getProductImages());
    assertEquals(productDTO.getProductItems(), product.getProductItems());
  }

  @Test
  public void testConvertToProductWithImages() {
    ProductDTO productDTO = new ProductDTO();
    List<ProductCategory> productCategoryDTOs = new ArrayList<>();
    List<ProductAttribute> productAttributeDTOs = new ArrayList<>();
    ProductImage productImageDataTransferObject = new ProductImage();
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setProduct(product);
    productImage.setProductId(PRODUCT_ID);
    productImage.setActive(true);
    List<ProductImage> productImageDTOs =
      Collections.singletonList(productImageDataTransferObject);
    List<ProductItem> productItemDTOs = new ArrayList<>();
    productDTO.setProductCategories(productCategoryDTOs);
    productDTO.setProductAttributes(productAttributeDTOs);
    productDTO.setProductImages(productImageDTOs);
    productDTO.setProductItems(productItemDTOs);
    Product product = ConverterUtil.convertProductDTOToProduct(productDTO);
    assertEquals(productDTO.getProductCategories(), product.getProductCategories());
    assertEquals(productDTO.getProductAttributes(), product.getProductAttributes());
    assertEquals(productDTO.getProductImages().get(0).getLocationPath(),
      product.getProductImages().get(0).getLocationPath());
    assertEquals(productDTO.getProductItems(), product.getProductItems());
  }

  @Test
  public void testConvertToProductWithAttributes() {
    ProductDTO productDTO = new ProductDTO();
    List<ProductCategory> productCategoryDTOs = new ArrayList<>();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    productAttributeDTO.setAttribute(attribute);
    productAttributeDTO.setProduct(product);
    productAttributeDTO.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    productAttributeDTO.setProductAttributeName(PRODUCT_NAME);
    List<ProductAttribute> productAttributeDTOs = Collections.singletonList(productAttribute);
    ProductImage productImageDataTransferObject = new ProductImage();
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setProduct(product);
    productImage.setProductId(PRODUCT_ID);
    productImage.setActive(true);
    List<ProductImage> productImageDTOs =
      Collections.singletonList(productImageDataTransferObject);
    List<ProductItem> productItemDTOs = new ArrayList<>();
    productDTO.setProductCategories(productCategoryDTOs);
    productDTO.setProductAttributes(productAttributeDTOs);
    productDTO.setProductImages(productImageDTOs);
    productDTO.setProductItems(productItemDTOs);
    Product product = ConverterUtil.convertProductDTOToProduct(productDTO);
    assertEquals(productDTO.getProductCategories(), product.getProductCategories());
    assertEquals(productDTO.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType(),
      product.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(productDTO.getProductImages().get(0).getLocationPath(),
      product.getProductImages().get(0).getLocationPath());
    assertEquals(productDTO.getProductItems(), product.getProductItems());
  }

//  @Test
//  @Disabled
//  public void testConvertToProductItem() {
//    ProductItemDTO productItemDTO = new ProductItemDTO();
//    ProductItemImageDTO productItemImageDTO =
//      ProductItemImageDTO.builder().productItemId(PRODUCT_ID).locationPath(LOCATION_PATH)
//        .commonImage(true).hashCode(LOCATION_PATH).build();
//    List<ProductItemImageDTO> productItemImageDTOs = Collections.singletonList(productItemImageDTO);
//    productItemDTO.setProductItemImageDTOS(productItemImageDTOs);
//    ProductItem productItem = ConverterUtil.convertToProductItem(productItemDTO);
//    assertEquals(productItem.getProductItemImages().get(0).getLocationPath(),
//      productItemDTO.getProductItemImageDTOS().get(0).getLocationPath());
//    assertEquals(productItem.getProductItemImages().get(0).getProductItemId(),
//      productItemDTO.getProductItemImageDTOS().get(0).getProductItemId());
//  }

  @Test
  public void testConvertProductToDTO() {
    product.setProductCode(PRODUCT_CODE);
    productImage.setProduct(product);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setProductImages(Collections.singletonList(productImage));
    ProductDTO productDTO = ConverterUtil.convertProductToDTO(product);
    assertEquals(productDTO.getId(), product.getId());
    assertEquals(productDTO.getProductImages().get(0).getProduct().getProductCode(),
      product.getProductImages().get(0).getProduct().getProductCode());
    assertEquals(productDTO.getProductImages().get(0).getLocationPath(),
      product.getProductImages().get(0).getLocationPath());
    assertEquals(productDTO.getDescription(), product.getDescription());
    assertEquals(productDTO.getProductItems().get(0).getProductId(),
      product.getProductItems().get(0).getProductId());
    assertEquals(productDTO.isRevised(), product.isRevised());
    assertEquals(productDTO.getProductAttributes().get(0).getAttribute().getAttributeCode(),
      product.getProductAttributes().get(0).getAttribute().getAttributeCode());
  }

  @Test
  public void testConvertProductToDTOWithCategory() {
    product.setProductCode(PRODUCT_CODE);
    productImage.setProduct(product);
    product.setProductCategories(null);
    product.setProductImages(Collections.singletonList(productImage));
    ProductDTO productDTO = ConverterUtil.convertProductToDTO(product);
    assertEquals(productDTO.getId(), product.getId());
    assertEquals(productDTO.getProductImages().get(0).getProduct().getProductCode(),
      product.getProductImages().get(0).getProduct().getProductCode());
    assertEquals(productDTO.getProductImages().get(0).getLocationPath(),
      product.getProductImages().get(0).getLocationPath());
    assertEquals(productDTO.getDescription(), product.getDescription());
    assertEquals(productDTO.getProductItems().get(0).getProductId(),
      product.getProductItems().get(0).getProductId());
    assertEquals(productDTO.isRevised(), product.isRevised());
    assertEquals(productDTO.getProductAttributes().get(0).getAttribute().getAttributeCode(),
      product.getProductAttributes().get(0).getAttribute().getAttributeCode());
  }

  @Test
  public void testConvertProductToDTO2() {
    product.setProductCode(PRODUCT_CODE);
    productImage.setLocationPath(LOCATION_PATH);
    product.setProductCode(PRODUCT_CODE);
    productImage.setProduct(product);
    product.getProductAttributes().forEach(productAttribute1 -> productAttribute1.setAttribute(null));
    product.getProductCategories().forEach(productCategory2 -> productCategory2.setCategory(null));
    product.setProductImages(Collections.singletonList(productImage));
    ProductDTO productDTO = ConverterUtil.convertProductToDTO(product);
    assertEquals(productDTO.getId(), product.getId());
    assertEquals(productDTO.getProductImages().get(0).getProduct().getProductCode(),
      product.getProductImages().get(0).getProduct().getProductCode());
    assertEquals(productDTO.getProductImages().get(0).getLocationPath(),
      product.getProductImages().get(0).getLocationPath());
    assertEquals(productDTO.getDescription(), product.getDescription());
    assertEquals(productDTO.getProductItems().get(0).getProductId(),
      product.getProductItems().get(0).getProductId());
    assertEquals(productDTO.isRevised(), product.isRevised());
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
    Assertions.assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, productAttributeValues.get(0).getDescriptiveAttributeValue());
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeEmptyListTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(null);
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListDescriptiveAttributeTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
    Assertions.assertNull(productAttributeValues.get(0).getDescriptiveAttributeValue());
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListPredefinedAttributeTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    AllowedAttributeValue allowedAttributeVal = new AllowedAttributeValue();
    allowedAttributeVal.setValue(ALLOWED_ATTRIBUTE_VALUE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(PREDEFINED_ATTRIBUTE_VALUE);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValue.setPredefinedAllowedAttributeValueId(ID);
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValue.setAllowedAttributeValueId(null);
    productAttributeValue.setAllowedAttributeValue(allowedAttributeVal);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
    Assertions.assertEquals(PREDEFINED_ATTRIBUTE_VALUE,
        productAttributeValues.get(0).getPredefinedAllowedAttributeValue().getValue());
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListPredefinedAttributeNullTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    AllowedAttributeValue allowedAttributeVal = new AllowedAttributeValue();
    allowedAttributeVal.setValue(null);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(null);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValue.setPredefinedAllowedAttributeValueId(ID);
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValue.setAllowedAttributeValueId(null);
    productAttributeValue.setAllowedAttributeValue(null);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListAllowedAttributeValueTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    AllowedAttributeValue allowedAttributeVal = new AllowedAttributeValue();
    allowedAttributeVal.setValue(ALLOWED_ATTRIBUTE_VALUE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(null);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValue.setPredefinedAllowedAttributeValueId(ID);
    productAttributeValue.setPredefinedAllowedAttributeValue(null);
    productAttributeValue.setAllowedAttributeValueId(ID);
    productAttributeValue.setAllowedAttributeValue(allowedAttributeVal);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_VALUE, productAttributeValue.getAllowedAttributeValue().getValue());
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListAllowedAttributeValueIdNullTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    AllowedAttributeValue allowedAttributeVal = new AllowedAttributeValue();
    allowedAttributeVal.setValue(ALLOWED_ATTRIBUTE_VALUE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(null);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValue.setPredefinedAllowedAttributeValueId(ID);
    productAttributeValue.setPredefinedAllowedAttributeValue(null);
    productAttributeValue.setAllowedAttributeValueId(null);
    productAttributeValue.setAllowedAttributeValue(null);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListAllowedAttributeValueNullTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    AllowedAttributeValue allowedAttributeVal = new AllowedAttributeValue();
    allowedAttributeVal.setValue(null);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(null);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValue.setPredefinedAllowedAttributeValueId(ID);
    productAttributeValue.setPredefinedAllowedAttributeValue(null);
    productAttributeValue.setAllowedAttributeValueId(ID);
    productAttributeValue.setAllowedAttributeValue(null);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
  }

  @Test
  public void getProductAttributeValuesFromProductAttributeListAllowedAttributeNullTest() {
    List<ProductAttributeValue> productAttributeValues = new ArrayList<>();
    AllowedAttributeValue allowedAttributeVal = new AllowedAttributeValue();
    allowedAttributeVal.setValue(null);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(null);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setDescriptiveAttributeValue(null);
    productAttributeValue.setPredefinedAllowedAttributeValueId(ID);
    productAttributeValue.setPredefinedAllowedAttributeValue(null);
    productAttributeValue.setAllowedAttributeValueId(ID);
    productAttributeValue.setAllowedAttributeValue(allowedAttributeVal);
    productAttributeValues.add(productAttributeValue);
    Product product = new Product();
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setId(ID);
    productAttribute.setAttribute(attribute);
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setProductId(product.getId());
    productAttribute.setProductAttributeValues(productAttributeValues);
    ConverterUtil.getProductAttributeValuesFromProductAttributeList(productAttributeValues);
  }

  @Test
  public void convertAttributeValueDTOToAttributeValueResponse() {
    Page<DimensionMappingResponse> response =
        ConverterUtil.toDimensionMappingResponse(dimensionMappingPage);
    assertNotNull(response);
  }

  @Test
  public void constructCategoryHistoryEventModelTest() {
    CategoryHistoryEventModel model =
      ConverterUtil.constructCategoryHistoryEventModel("true", "false", CATEGORY_CODE, STORE_ID,
        USER_NAME, ACTIVITY);
    Assertions.assertEquals("false", model.getNewStatus());
  }

  @Test
  public void doCacheClearCategoryTreeTest() {
    Assertions.assertTrue(ConverterUtil.doCacheClearCategoryTree(true, false, false, false));
    Assertions.assertTrue(ConverterUtil.doCacheClearCategoryTree(false, true, false, false));
    Assertions.assertTrue(ConverterUtil.doCacheClearCategoryTree(false, false, true, false));
    Assertions.assertTrue(ConverterUtil.doCacheClearCategoryTree(false, false, false, true));
    Assertions.assertFalse(ConverterUtil.doCacheClearCategoryTree(false, false, false, false));
  }

  @Test
  public void getAttributeValueFromValueAndValueTypeTest() {
    Assertions.assertEquals(Arrays.asList("S", "L"),
        ConverterUtil.getAttributeValueFromValueAndValueType(Arrays.asList("UK-S", "UK-L"), SIZE_CHART_DELIMITER));
  }

  @Test
  public void populateDefiningAttributeTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    AllowedAttributeValue allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setAttribute(attribute1);
    allowedAttributeValue1.setId(ID);

    Map<String, Map<String, Map<String, AllowedAttributeValue>>> sanitizedAttributeCodeAndValueAndValueTypeMapFromDb =
        Map.of(ATTRIBUTE_CODE, Map.of("S", Map.of("UK", allowedAttributeValue1)));

    AllowedAttributeValueDtoRequest allowedAttributeValueDtoRequest1 = new AllowedAttributeValueDtoRequest();
    allowedAttributeValueDtoRequest1.setAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueDtoRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    allowedAttributeValueDtoRequest1.setValues(Arrays.asList("UK-S", "S", "UK-L"));

    AllowedAttributeValueDtoRequest allowedAttributeValueDtoRequest2 = new AllowedAttributeValueDtoRequest();
    allowedAttributeValueDtoRequest2.setAttributeCode(ATTRIBUTE_CODE_2);

    List<AllowedAttributeValueDtoResponse> result = ConverterUtil.populateAllowedAttributeSizeChartValue(
        Arrays.asList(allowedAttributeValueDtoRequest1, allowedAttributeValueDtoRequest2),
        sanitizedAttributeCodeAndValueAndValueTypeMapFromDb, SIZE_CHART_DELIMITER);
    Assertions.assertEquals(ID, result.getFirst().getAllowedValue().getFirst().getAllowedValueId());

    allowedAttributeValueDtoRequest1.setValues(Arrays.asList("UK-S", "US-S", "UK-L"));
    ConverterUtil.populateAllowedAttributeSizeChartValue(
        Arrays.asList(allowedAttributeValueDtoRequest1, allowedAttributeValueDtoRequest2),
        sanitizedAttributeCodeAndValueAndValueTypeMapFromDb, SIZE_CHART_DELIMITER);
    Assertions.assertEquals(ID, result.getFirst().getAllowedValue().getFirst().getAllowedValueId());


    sanitizedAttributeCodeAndValueAndValueTypeMapFromDb =
        Map.of(ATTRIBUTE_CODE, Map.of("S", Map.of("UK", allowedAttributeValue1, "US", new AllowedAttributeValue())));
    allowedAttributeValueDtoRequest1.setValues(Arrays.asList("UK-S", "S", "UK-L"));
    ConverterUtil.populateAllowedAttributeSizeChartValue(
        Arrays.asList(allowedAttributeValueDtoRequest1, allowedAttributeValueDtoRequest2),
        sanitizedAttributeCodeAndValueAndValueTypeMapFromDb, SIZE_CHART_DELIMITER);
    Assertions.assertEquals(ID, result.getFirst().getAllowedValue().getFirst().getAllowedValueId());
  }

  @Test
  public void populatePredefinedAttributeTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue1 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue1.setAttribute(attribute1);
    predefinedAllowedAttributeValue1.setId(ID);
    List<AllowedAttributeValueDtoResponse> result =
        ConverterUtil.populatePredefinedAttribute(List.of(predefinedAllowedAttributeValue1));
    Assertions.assertEquals(ID, result.getFirst().getAllowedValue().getFirst().getAllowedValueId());
  }

  @Test
  public void getAttributeCodeAndValueAndValueTypeMapFromDbTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    AllowedAttributeValue allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setAttribute(attribute);
    allowedAttributeValue1.setValue("S");
    allowedAttributeValue1.setValueType("UK");
    Map<String, Map<String, Map<String, AllowedAttributeValue>>> result =
        ConverterUtil.getAttributeCodeAndValueAndValueTypeMapFromDb(List.of(allowedAttributeValue1));
    Assertions.assertEquals(allowedAttributeValue1, result.get(ATTRIBUTE_CODE).get("S").get("UK"));
  }

  @Test
  public void groupByAttributeTypeTest() {
    AllowedAttributeValueDtoRequest allowedAttributeValueDtoRequest1 = new AllowedAttributeValueDtoRequest();
    allowedAttributeValueDtoRequest1.setAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueDtoRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    allowedAttributeValueDtoRequest1.setValues(List.of(VALUE));

    AllowedAttributeValueDtoRequest allowedAttributeValueDtoRequest2 = new AllowedAttributeValueDtoRequest();
    allowedAttributeValueDtoRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    allowedAttributeValueDtoRequest2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    allowedAttributeValueDtoRequest2.setValues(List.of(VALUE2));

    AllowedAttributeValueDtoRequest allowedAttributeValueDtoRequest3 = new AllowedAttributeValueDtoRequest();
    allowedAttributeValueDtoRequest3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());

    List<String> predefinedValues = new ArrayList<>();
    List<String> predefinedCodes = new ArrayList<>();
    List<String> definingValues = new ArrayList<>();
    List<String> definingCodes = new ArrayList<>();
    List<AllowedAttributeValueDtoRequest> definingAttributeRequest = new ArrayList<>();

    ConverterUtil.groupByAttributeType(
        List.of(allowedAttributeValueDtoRequest1, allowedAttributeValueDtoRequest2, allowedAttributeValueDtoRequest3),
        predefinedValues, predefinedCodes, definingValues, definingCodes, definingAttributeRequest);

    Assertions.assertEquals(ATTRIBUTE_CODE, predefinedCodes.getFirst());
    Assertions.assertEquals(VALUE, predefinedValues.getFirst());
    Assertions.assertEquals(ATTRIBUTE_CODE_2, definingCodes.getFirst());
    Assertions.assertEquals(VALUE2, definingValues.getFirst());
    Assertions.assertEquals(allowedAttributeValueDtoRequest2, definingAttributeRequest.getFirst());
  }

  @Test
  public void convertProductItemToBasicItemDetailResponseTest() {
    productItem.setGeneratedItemName(ITEM_NAME);
    List<ItemImageResponse> itemImageResponses =
      ConverterUtil.convertProductItemToBasicItemDetailResponse(Collections.singletonList(productItem));
    Assertions.assertEquals(productItem.getSkuCode(), itemImageResponses.get(0).getItemCode());
    Assertions.assertEquals(ITEM_NAME,
      itemImageResponses.get(0).getItemName());
  }

  @Test
  public void groupBySizeChartAttributeTest() {
    List<AllowedAttributeValue> sizeChartAllowedAttributeValue = new ArrayList<>();
    List<AllowedAttributeValue> nonSizeChartAllowedAttributeValue = new ArrayList<>();
    Attribute attribute1 = new Attribute();
    attribute1.setSizeAttribute(true);
    Attribute attribute2 = new Attribute();
    AllowedAttributeValue allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setAttribute(attribute1);
    AllowedAttributeValue allowedAttributeValue2 = new AllowedAttributeValue();
    allowedAttributeValue2.setAttribute(attribute2);
    ConverterUtil.groupBySizeChartAttribute(Arrays.asList(allowedAttributeValue1, allowedAttributeValue2),
        sizeChartAllowedAttributeValue, nonSizeChartAllowedAttributeValue);
    Assertions.assertEquals(allowedAttributeValue1, sizeChartAllowedAttributeValue.getFirst());
    Assertions.assertEquals(allowedAttributeValue2, nonSizeChartAllowedAttributeValue.getFirst());
  }

  @Test
  public void populateAllowedAttributeNonSizeChartValueTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    AllowedAttributeValue allowedAttributeValue1 = new AllowedAttributeValue();
    allowedAttributeValue1.setAttribute(attribute);
    allowedAttributeValue1.setValue("S");
    List<AllowedAttributeValueDtoResponse> result = ConverterUtil.populateAllowedAttributeNonSizeChartValue(
        List.of(allowedAttributeValue1));
    Assertions.assertEquals("S", result.getFirst().getAllowedValue().getFirst().getValue());
  }

  @Test
  void toBrandWipHistoryModelTest() {
    BrandHistoryEventModel historyEventModel =
      ConverterUtil.toBrandWipHistoryModel(brandWip, USERNAME);
    Assertions.assertEquals(USERNAME, historyEventModel.getUsername());
  }

  @Test
  public void toBasicInfoProductResponse() {
    Image image = new Image();
    product.setDescription(DESCRIPTION.getBytes());
    BasicInfoProductResponse basicInfoProductResponse1 =
        ConverterUtil.toBasicInfoProductResponse(Collections.singletonList(image), product);
    Assertions.assertEquals(product.getName(), basicInfoProductResponse1.getProductName());
  }

  @Test
  void convertVideoAddEditRequestToDTOTest(){
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoUrl("url");
    String result = ConverterUtil.convertVideoAddEditRequestToDTO(videoAddEditRequest);
    assertTrue(result.contains("url"));
  }

  @Test
  void convertVideoAddEditRequestToDTOExceptionTest(){
    String result = ConverterUtil.convertVideoAddEditRequestToDTO(null);
    assertTrue(result.isEmpty());
  }
}
