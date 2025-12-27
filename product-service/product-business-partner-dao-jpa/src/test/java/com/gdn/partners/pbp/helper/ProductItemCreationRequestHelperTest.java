package com.gdn.partners.pbp.helper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.x.productcategorybase.dto.VideoDTO;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductItemsImageUpdateRequest;
import com.gda.mta.product.dto.ProductL3CommonImageRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ProductItemCreationRequestHelperTest {

  private static final String STORE_ID = "STORE_ID";

  private static final String PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";

  private static final String ITEM_CODE = "ITEM_CODE";

  private static final String ITEM_SKU = "ITEM_SKU";

  private static final String PRODUCT_BRAND = "PRODUCT_BRAND";

  private static final String PRODUCT_NAME = "PRODUCT_NAME";

  private static final String PRODUCT_CODE = "PRODUCT_CODE";

  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";

  private static final String CATEGORY_NAME = "CATEGORY_NAME";

  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";

  private static final double PRODUCT_ITEM_PRICE = 1000000000;

  private static final double PRODUCT_ITEM_SALE_PRICE = 345435435;

  private static final String ITEM_NAME = "ITEM-NAME";

  private static final String UPC_CODE = "121212212";

  private static final String ATTRIBUTE_ID = "attributeResponse-id";

  private static final String ATTRIBUTE_CODE_1 = "attribute-code-1";

  private static final String ATTRIBUTE_ID_2 = "attribute-id-2";

  private static final String ATTRIBUTE_VALUE_1 = "attribute-value";

  private static final String ATTRIBUTE_VALUE_2 = "attribute-value-2";

  private static final String ATTRIBUTE_NAME_1 = "warna";

  private static final String IMAGE_ID = "image-id-1";

  private static final String PRODUCT_STORY = "product-story";

  private static final String SPECIFICATION_DETAIL = "specification-detail";

  private static final String BRAND_CODE = "BRD-01608";

  private static final String URL = "youtube-url";

  private static final String USER_1 = "user-1";

  private static final String USER_2 = "user-2";

  private static final String CATEGORY_ID = "category-id";

  private static final String CATEGORY_CODE = "PO-1000038";

  private static final String CATEGORY_NAME_1 = "Postlive Category 3";

  private static final String PARENT_CATEGORY_ID = "parent-category-id";

  private static final String CATALOG_TYPE = "MASTER_CATALOG";

  private static final String CATALOG_ID = "41591a5a-daad-11e4-b9d6-1681e6b88ec1";

  private static final String CATALOG_NAME = "MASTER CATALOG";

  private static final String CATALOG_CODE = "10001";

  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptive-attribute-value";

  private static final String PREDEFINE_ATTRIBUTE_VALUE = "predefined-allowed-attribute-value";

  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";

  private static final String ALLOWED_ATTRIBUTE_VALUE = "allowed-attribute-value";

  private static final String FAMILY_COLOR_ATTRIBUTE_CODE = "FA-2000033";

  private static final String FAMILY_COLOR_ATTRIBUTE = "Family Colour";

  private static final String SKU_CODE = "SKU-CODE";

  private static final String NEW = "new";

  private static final String UPDATE = "update";

  private static final String DEFAULT_LOCATION_PATH = "image.jpg";
  public static final String VIDEO_ID = "video-id";
  public static final String VIDEO_URL = "video-url";
  public static final String COVER_IMAGE_PATH = "cover-image";

  private ProfileResponse businessPartnerProfile;
  private ProductL3UpdateRequest productL3UpdateRequest;
  private ProductVariantUpdateRequest productVariantUpdateRequest;
  private ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest;
  private ProductLevel3SummaryDetailsImageRequest imageDetailsRequest;
  private List<ProductVariantPriceStockAndImagesRequest> stockAndImagesRequestList;
  private List<ProductLevel3SummaryDetailsImageRequest> images;
  private ProductL3CommonImageRequest l3CommonImageRequest;
  private List<ProductL3CommonImageRequest> productL3CommonImageRequestList;

  @BeforeEach
  public void init() {
    this.businessPartnerProfile = ProfileResponse.builder()
      .merchantStatus(Constants.DEFAULT)
      .company(CompanyDTO.builder().businessPartnerName("CM-NEW-MERCHANT")
        .purchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER).build())
      .build();
    businessPartnerProfile.setId("businessPartnerProfile-id");

    imageDetailsRequest = new ProductLevel3SummaryDetailsImageRequest();
    imageDetailsRequest.setSequence(0);
    imageDetailsRequest.setReviewType(NEW);
    imageDetailsRequest.setMarkForDelete(false);
    imageDetailsRequest.setLocationPath(DEFAULT_LOCATION_PATH);
    imageDetailsRequest.setMainImage(true);

    productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequest.setProductSku(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setImages(images);


    productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductCode(PRODUCT_CODE);
    productL3UpdateRequest.setNeedCorrection(true);

    l3CommonImageRequest = new ProductL3CommonImageRequest();
    l3CommonImageRequest.setLocationPath(DEFAULT_LOCATION_PATH);
    l3CommonImageRequest.setMainImage(true);
    l3CommonImageRequest.setMarkForDelete(false);
    l3CommonImageRequest.setReviewType(NEW);
    l3CommonImageRequest.setSequence(0);

    stockAndImagesRequestList = new ArrayList<>();
    stockAndImagesRequestList.add(productVariantPriceStockAndImagesRequest);

    productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductItems(stockAndImagesRequestList);

    productL3CommonImageRequestList = new ArrayList<>();
    productL3CommonImageRequestList.add(l3CommonImageRequest);
    productL3UpdateRequest.setCommonImages(Arrays.asList(imageDetailsRequest));
  }

  @Test
  public void createProductItemRequestPayload() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode(UPC_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_2);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINE_ATTRIBUTE_VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attributeResponse-id", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("attribute-code-1", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
        .getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals("predefined-allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attributeResponse-id", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals("predefined-allowed-attribute-value", request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals(StringUtils.EMPTY, request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
  }

  @Test
  public void createProductItemRequestPayload_whenProductAttributeResponseIsNull() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode("12121");

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));
    productData.setProductAttributeResponses(null);

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(0, request.getProductBusinessPartnerAttributes().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals("12121", request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
  }

  @Test
  public void createProductItemRequestPayload_whenProductCategoryResponseIsNull() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode("upc-code");

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_2);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINE_ATTRIBUTE_VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    productData.setProductCategoryResponses(null);

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals(URL, request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attributeResponse-id", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("attribute-code-1", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
        .getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals("predefined-allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(0, request.getProductCategories().size());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attributeResponse-id", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals("predefined-allowed-attribute-value", request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals(StringUtils.EMPTY, request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
  }

  @Test
  public void createProductItemRequestPayload_whenProductAttributeResponsesIsNull() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode("12121212");

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(null);

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_1);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINE_ATTRIBUTE_VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals(URL, request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attributeResponse-id", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("attribute-code-1", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
        .getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals("predefined-allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attributeResponse-id", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals("predefined-allowed-attribute-value", request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals("12121212", request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
  }

  @Test
  public void createProductItemRequestPayload_whenPredefinedAllowedAttributeValueIsNull() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode(UPC_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DEFINING_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(false);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_1);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attributeResponse-id", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("attribute-code-1", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.DEFINING_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
        .getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals(null,
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attributeResponse-id", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals(StringUtils.EMPTY, request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals(StringUtils.EMPTY, request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
  }

  @Test
  public void createProductItemRequestPayload_whenAttributesMapIsNotApplicable() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode(UPC_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(false);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_2);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attributeResponse-id", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("attribute-code-1", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
        .getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals(null,
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attributeResponse-id", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals(StringUtils.EMPTY, request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals(StringUtils.EMPTY, request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
  }

  @Test
  public void createProductItemRequestPayload_whenAllowedValidAttributeCodeData() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode(UPC_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse_1 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse_1 = new AttributeResponse();
    attributeResponse_1.setId(ATTRIBUTE_ID);
    attributeResponse_1.setSearchAble(true);
    attributeResponse_1.setSkuValue(true);
    attributeResponse_1.setMandatory(false);
    attributeResponse_1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse_1.setName(ATTRIBUTE_NAME_1);
    attributeResponse_1.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse_1.setBasicView(true);
    attributeResponse_1.setVariantCreation(true);
    productItemAttributeValueResponse_1.setAttributeResponse(attributeResponse_1);
    productItemAttributeValueResponse_1.setValue(ATTRIBUTE_VALUE_1);

    ProductItemAttributeValueResponse productItemAttributeValueResponse_2 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse_2 = new AttributeResponse();
    attributeResponse_2.setId(ATTRIBUTE_ID_2);
    attributeResponse_2.setSearchAble(true);
    attributeResponse_2.setSkuValue(true);
    attributeResponse_2.setMandatory(false);
    attributeResponse_2.setAttributeCode(FAMILY_COLOR_ATTRIBUTE_CODE);
    attributeResponse_2.setName(FAMILY_COLOR_ATTRIBUTE);
    attributeResponse_2.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponse_2.setBasicView(true);
    attributeResponse_2.setVariantCreation(true);
    productItemAttributeValueResponse_2.setAttributeResponse(attributeResponse_2);
    productItemAttributeValueResponse_2.setValue(ATTRIBUTE_VALUE_2);
    itemResponse.setProductItemAttributeValueResponses(
      Arrays.asList(productItemAttributeValueResponse_1, productItemAttributeValueResponse_2));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_2);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse_2.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINE_ATTRIBUTE_VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse_2.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse_2);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attribute-id-2", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("FA-2000033", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.PREDEFINED_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0)
        .getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals("predefined-allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attribute-id-2", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals("predefined-allowed-attribute-value", request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals(StringUtils.EMPTY, request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(1, request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().size());
    assertEquals("FA-2000033",
      request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0).getAttribute()
        .getAttributeCode());
    assertEquals("Family Colour",
      request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0).getAttribute()
        .getName());
  }

  @Test
  public void createProductItemRequestPayload_whenAllowedAttributeValuesAndPredefinedAllowedAttributeValuesIsNull() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode(UPC_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse_1 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse_1 = new AttributeResponse();
    attributeResponse_1.setId(ATTRIBUTE_ID);
    attributeResponse_1.setSearchAble(true);
    attributeResponse_1.setSkuValue(true);
    attributeResponse_1.setMandatory(false);
    attributeResponse_1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse_1.setName(ATTRIBUTE_NAME_1);
    attributeResponse_1.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse_1.setBasicView(true);
    attributeResponse_1.setVariantCreation(true);
    productItemAttributeValueResponse_1.setAttributeResponse(attributeResponse_1);
    productItemAttributeValueResponse_1.setValue(ATTRIBUTE_VALUE_1);

    ProductItemAttributeValueResponse productItemAttributeValueResponse_2 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse_2 = new AttributeResponse();
    attributeResponse_2.setId(ATTRIBUTE_ID_2);
    attributeResponse_2.setSearchAble(true);
    attributeResponse_2.setSkuValue(true);
    attributeResponse_2.setMandatory(false);
    attributeResponse_2.setAttributeCode(FAMILY_COLOR_ATTRIBUTE_CODE);
    attributeResponse_2.setName(FAMILY_COLOR_ATTRIBUTE);
    attributeResponse_2.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponse_2.setBasicView(true);
    attributeResponse_2.setVariantCreation(true);
    productItemAttributeValueResponse_2.setAttributeResponse(attributeResponse_2);
    productItemAttributeValueResponse_2.setValue(ATTRIBUTE_VALUE_2);
    itemResponse.setProductItemAttributeValueResponses(
      Arrays.asList(productItemAttributeValueResponse_1, productItemAttributeValueResponse_2));

    Image image = new Image();
    image.setId(IMAGE_ID);
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_2);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINE_ATTRIBUTE_VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    productAttributeResponse.setAttribute(attributeResponse_2);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("STORE_ID", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(new Integer(1), request.getProductAttributes().get(0).getSequence());
    assertEquals("attribute-id-2", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("FA-2000033", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.PREDEFINED_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attribute-id-2", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals("predefined-allowed-attribute-value", request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("ITEM-NAME", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertEquals(StringUtils.EMPTY, request.getProductItemRequests().get(0).getUpcCode());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(1, request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().size());
    assertEquals("FA-2000033",
      request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0).getAttribute()
        .getAttributeCode());
  }

  @Test
  public void createProductItemRequestPayload_whenImageActiveFlagIsFalse_thenSetActiveFlagTrue() {
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName(ITEM_NAME);
    itemResponse.setSourceItemCode(ITEM_CODE);
    itemResponse.setUpcCode(UPC_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ATTRIBUTE_ID);
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME_1);
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTRIBUTE_VALUE_1);
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image_1 = new Image();
    image_1.setId(IMAGE_ID);
    image_1.setActive(false);

    Image image_2 = new Image();
    image_2.setId(IMAGE_ID);
    image_2.setActive(false);
    itemResponse.setImages(Arrays.asList(image_1, image_2));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory(PRODUCT_STORY);
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail(SPECIFICATION_DETAIL);
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl(URL);
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode(BRAND_CODE);
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId(STORE_ID);
    productAttributeResponse.setUpdatedBy(USER_1);
    productAttributeResponse.setCreatedBy(USER_2);
    productAttributeResponse.setProductAttributeName(ATTRIBUTE_NAME_1);
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINE_ATTRIBUTE_VALUE);
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId(CATEGORY_ID);
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME_1);
    category.setParentCategoryId(PARENT_CATEGORY_ID);
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId(CATALOG_ID);
    catalog.setCatalogCode(CATALOG_CODE);
    catalog.setCatalogType(CATALOG_TYPE);
    catalog.setName(CATALOG_NAME);
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap = new HashMap<>();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    productItemsBusinessPartnerMap.put(PRODUCT_ITEM_ID, productItemBusinessPartner);
    ProductCreationRequest request = ProductItemCreationRequestHelper
      .createProductItemRequestPayload(productData, businessPartnerProfile, productItemsBusinessPartnerMap,
        ProductCreationType.FLOW2_FBB, false,
        "MTA-00001", PICKUP_POINT_CODE, CATEGORY_NAME, BUSINESS_PARTNER_CODE);

    Assertions.assertNotNull(request);
    assertEquals(2, request.getProductItemRequests().get(0).getImages().size());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertTrue(request.getProductItemRequests().get(0).getImages().get(0).isActive());
    assertNull(request.getProductItemRequests().get(0).getImages().get(1).getId());
    assertTrue(request.getProductItemRequests().get(0).getImages().get(1).isActive());
  }

  @Test
  public void generateProductLevel3RequestRegularTest() {
    ProfileResponse profileResponse = getBusinessPartnerProfile();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFbbActivated(true);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setFbbActive(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Map<String, AttributeResponse> attributeResponseMap = getAttributeResponseMap();
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap = getProductItemWholesalePriceMap();
    ProductAndItemActivationRequest productAndItemActivationRequest = ProductItemCreationRequestHelper
        .generateProductLevel3Request(profileResponse, productDetailResponse, productBusinessPartner,
            attributeResponseMap, productItemWholesalePriceMap, false);
    assertEquals(BUSINESS_PARTNER_CODE, productAndItemActivationRequest.getProduct().getMerchantCode());
    assertEquals(ProductType.REGULAR, productAndItemActivationRequest.getProduct().getProductType());
    assertEquals(1, productAndItemActivationRequest.getProduct().getProductSpecialAttributes().size());
    Assertions.assertTrue(productAndItemActivationRequest.getProduct().isTradingProduct());
    assertEquals(1000.0,
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getPrice().iterator().next()
            .getListPrice(), 0);
    assertEquals(1000.0,
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getPrice().iterator().next()
            .getOfferPrice(), 0);
    Assertions.assertTrue(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.DEFAULT.equals(itemViewConfig.getChannel())).iterator().next().isBuyable());
    Assertions.assertTrue(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.DEFAULT.equals(itemViewConfig.getChannel())).iterator().next().isDiscoverable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B_CHANNEL.equals(itemViewConfig.getChannel())).iterator().next().isBuyable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B_CHANNEL.equals(itemViewConfig.getChannel())).iterator().next().isDiscoverable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getProduct().isOnline());
    Assertions.assertTrue(productAndItemActivationRequest.getProduct().isFbbActivated());
    Assertions.assertTrue(productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).isFbbActivated());
    Assertions.assertEquals(VIDEO_ID,
        productAndItemActivationRequest.getProduct().getVideoAddEditRequest().getVideoId());
  }

  @Test
  public void generateProductLevel3RequestRegularMppTrueTest() {
    ProfileResponse profileResponse = getBusinessPartnerProfile();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Map<String, AttributeResponse> attributeResponseMap = getAttributeResponseMap();
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap = getProductItemWholesalePriceMapMppOn();
    ProductAndItemActivationRequest productAndItemActivationRequest = ProductItemCreationRequestHelper
        .generateProductLevel3Request(profileResponse, productDetailResponse, productBusinessPartner,
            attributeResponseMap, productItemWholesalePriceMap, false);
    assertEquals(BUSINESS_PARTNER_CODE, productAndItemActivationRequest.getProduct().getMerchantCode());
    assertEquals(ProductType.REGULAR, productAndItemActivationRequest.getProduct().getProductType());
    assertEquals(1, productAndItemActivationRequest.getProduct().getProductSpecialAttributes().size());
    Assertions.assertTrue(productAndItemActivationRequest.getProduct().isTradingProduct());
    assertEquals(1000.0,
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getPrice().iterator().next()
            .getListPrice(), 0);
    assertEquals(1000.0,
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getPrice().iterator().next()
            .getOfferPrice(), 0);
    Assertions.assertTrue(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.DEFAULT.equals(itemViewConfig.getChannel())).iterator().next().isBuyable());
    Assertions.assertTrue(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.DEFAULT.equals(itemViewConfig.getChannel())).iterator().next().isDiscoverable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B_CHANNEL.equals(itemViewConfig.getChannel())).iterator().next().isBuyable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B_CHANNEL.equals(itemViewConfig.getChannel())).iterator().next().isDiscoverable());
    Assertions.assertTrue(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).isWholesalePriceExists());
    Assertions.assertFalse(
        productAndItemActivationRequest.getProduct().isOnline());
  }


  @Test
  public void generateProductLevel3RequestRegularMppTrueTestMarkForDeleteTrue() {
    ProfileResponse profileResponse = getBusinessPartnerProfile();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setMarkForDelete(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Map<String, AttributeResponse> attributeResponseMap = getAttributeResponseMap();
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap = getProductItemWholesalePriceMapMppOn();
    ProductAndItemActivationRequest productAndItemActivationRequest = ProductItemCreationRequestHelper
        .generateProductLevel3Request(profileResponse, productDetailResponse, productBusinessPartner,
            attributeResponseMap, productItemWholesalePriceMap, false);
    assertEquals(BUSINESS_PARTNER_CODE, productAndItemActivationRequest.getProduct().getMerchantCode());
    assertEquals(ProductType.REGULAR, productAndItemActivationRequest.getProduct().getProductType());
    assertEquals(1, productAndItemActivationRequest.getProduct().getProductSpecialAttributes().size());
    Assertions.assertTrue(productAndItemActivationRequest.getProduct().isTradingProduct());
    assertEquals(new ArrayList<>(),productAndItemActivationRequest.getItems().get(0).getItemPickupPoints());
    Assertions.assertFalse(
        productAndItemActivationRequest.getProduct().isOnline());
  }

  @Test
  public void generateProductLevel3RequestBigProductTest() throws Exception {
    ProfileResponse profileResponse = getBusinessPartnerProfile();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setOnline(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Map<String, AttributeResponse> attributeResponseMap = getAttributeResponseMap();
    profileResponse.getCompany().setInventoryFulfillment(null);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(2);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setBundleRecipe("{}");
    ProductAndItemActivationRequest productAndItemActivationRequest =
        ProductItemCreationRequestHelper.generateProductLevel3Request(profileResponse, productDetailResponse,
            productBusinessPartner, attributeResponseMap, new HashMap<>(), true);
    assertEquals(BUSINESS_PARTNER_CODE, productAndItemActivationRequest.getProduct().getMerchantCode());
    assertEquals(ProductType.BIG_PRODUCT, productAndItemActivationRequest.getProduct().getProductType());
    Assertions.assertFalse(productAndItemActivationRequest.getProduct().isTradingProduct());
    assertEquals(1000.0,
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getPrice().iterator().next()
            .getListPrice(), 0);
    assertEquals(1000.0,
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getPrice().iterator().next()
            .getOfferPrice(), 0);
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().iterator()
            .next().isBuyable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getItemViewConfigs().iterator()
            .next().isDiscoverable());
    Assertions.assertFalse(
        productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).isWholesalePriceExists());
    Assertions.assertTrue(
        productAndItemActivationRequest.getProduct().isOnline());
  }

  @Test
  public void generateProductLevel3RequestBopisTest() throws Exception {
    ProfileResponse profileResponse = getBusinessPartnerProfile();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setB2bManaged(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Map<String, AttributeResponse> attributeResponseMap = getAttributeResponseMap();
    profileResponse.getCompany().setPurchaseTerm(null);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(3);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setBundleRecipe("[{\"itemSku\":\"itemSku\",\"quantity\":1}]");
    productDetailResponse.getProductItemResponses().iterator().next().setId("ID");
    ProductAndItemActivationRequest productAndItemActivationRequest =
        ProductItemCreationRequestHelper.generateProductLevel3Request(profileResponse, productDetailResponse,
            productBusinessPartner, attributeResponseMap, new HashMap<>(), false);
    assertEquals(BUSINESS_PARTNER_CODE, productAndItemActivationRequest.getProduct().getMerchantCode());
    Assertions.assertFalse(productAndItemActivationRequest.getProduct().isTradingProduct());
    assertEquals(ProductType.BOPIS, productAndItemActivationRequest.getProduct().getProductType());
    Assertions.assertTrue(productAndItemActivationRequest.getItems().get(0).getItemPickupPoints().get(0).getB2bFields().isManaged());
  }

  @Test
  public void createImageUpdateRequestTest() throws Exception {
    ProductItemsImageUpdateRequest request = ProductItemCreationRequestHelper
      .createImageUpdateRequest(productL3UpdateRequest, productVariantUpdateRequest);
    assertEquals(PRODUCT_CODE,request.getProductCode());
  }

  @Test
  public void createImageUpdateRequest_EmptyCollectionsTest() throws Exception {
    productVariantUpdateRequest.setProductItems(new ArrayList<>());
    productL3UpdateRequest.setCommonImages(new ArrayList<>());
    ProductItemsImageUpdateRequest request = ProductItemCreationRequestHelper
      .createImageUpdateRequest(productL3UpdateRequest, productVariantUpdateRequest);
    assertEquals(PRODUCT_CODE,request.getProductCode());
  }

  private ProductDetailResponse getProductDetailResponse() {
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ITEM_ID);
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));
    VideoDTO videoDTO = new VideoDTO();
    videoDTO.setVideoId(VIDEO_ID);
    videoDTO.setSourceUrl(VIDEO_URL);
    videoDTO.setCoverImagePath(COVER_IMAGE_PATH);
    productDetailResponse.setVideoDTO(videoDTO);
    return productDetailResponse;
  }

  private ProductBusinessPartner getProductBusinessPartner() {
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductType(1);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner1.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner1.setInstallation(true);
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner1.setSalePrice(1000.0);
    productItemBusinessPartner1.setPrice(1000.0);
    productItemBusinessPartner1.setBuyable(true);
    productItemBusinessPartner1.setDisplay(true);

    ProductBusinessPartnerAttribute productBusinessPartnerAttribute1 = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute1.setAttributeId(ATTRIBUTE_ID);
    productBusinessPartnerAttribute1.setValue(ATTRIBUTE_VALUE_1);

    ProductBusinessPartnerAttribute productBusinessPartnerAttribute2 = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute2.setAttributeId(ATTRIBUTE_ID_2);

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner1));
    productBusinessPartner.setProductBusinessPartnerAttributes(Arrays.asList(productBusinessPartnerAttribute1, productBusinessPartnerAttribute2));

    return productBusinessPartner;
  }

  private ProfileResponse getBusinessPartnerProfile() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    companyDTO.setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    return profileResponse;
  }

  private Map<String, AttributeResponse> getAttributeResponseMap(){
    return ImmutableMap.of(ATTRIBUTE_ID, new AttributeResponse());
  }

  private Map<String, ProductItemWholesalePrice> getProductItemWholesalePriceMap() {
    return ImmutableMap.of(ITEM_SKU, new ProductItemWholesalePrice());
  }

  private Map<String, ProductItemWholesalePrice> getProductItemWholesalePriceMapMppOn() {
    return ImmutableMap.of(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE, new ProductItemWholesalePrice());
  }

  @Test
  public void validateProductItemsEmptyListTest(){
    productVariantUpdateRequest.setProductCode(PRODUCT_CODE);
    productVariantUpdateRequest.setProductSku(SKU_CODE);
    productVariantUpdateRequest.setProductItems(new ArrayList<>());
    ProductVariantUpdateRequest result =
      ProductItemCreationRequestHelper.validateProductItems(productVariantUpdateRequest);
    assertEquals(1,result.getProductItems().size());
    assertEquals(PRODUCT_CODE,result.getProductItems().get(0).getProductCode());
  }

  @Test
  public void validateProductItemsTest(){
    productVariantUpdateRequest.setProductCode(PRODUCT_CODE);
    productVariantUpdateRequest.setProductSku(SKU_CODE);
    productVariantUpdateRequest.getProductItems().get(0).setProductCode(null);
    productVariantUpdateRequest.getProductItems().get(0).setProductSku(null);
    ProductVariantUpdateRequest result =
      ProductItemCreationRequestHelper.validateProductItems(productVariantUpdateRequest);
    assertEquals(1,result.getProductItems().size());
    assertEquals(PRODUCT_CODE,result.getProductItems().get(0).getProductCode());
  }

  @Test
  public void validateProductItemsValidTest(){
    productVariantUpdateRequest.setProductCode(PRODUCT_CODE);
    productVariantUpdateRequest.setProductSku(SKU_CODE);
    productVariantUpdateRequest.setProductItems(stockAndImagesRequestList);
    ProductVariantUpdateRequest result =
      ProductItemCreationRequestHelper.validateProductItems(productVariantUpdateRequest);
    assertEquals(1,result.getProductItems().size());
    assertEquals(PRODUCT_CODE,result.getProductItems().get(0).getProductCode());
  }

}
