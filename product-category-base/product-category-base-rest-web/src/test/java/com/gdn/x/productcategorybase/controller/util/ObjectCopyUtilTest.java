package com.gdn.x.productcategorybase.controller.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public class ObjectCopyUtilTest {

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String CREATED_BY = "CREATED_BY";

  private static final String UPDATED_BY = "updated-by";
  private static final String PRODUCT_ITEM_NAME = "PRODUCT_ITEM_NAME";
  private static final String PRODUCT_ITEM_NAME_2 = "PRODUCT_ITEM_NAME_2";
  private static final String PRODUCT_ITEM_NAME_SEARCH = "item_name";
  private static final String SKU_CODE_2 = "SKU_CODE_2";
  private static final String UPC_CODE_2 = "UPC_CODE_2";
  private static final String SKU_CODE_1 = "SKU_CODE_1";
  private static final String UPC_CODE_1 = "UPC_CODE_1";
  private static final String WRONG_SKU_CODE = "WRONG_SKU_CODE";
  private static final String UPC_CODE_SEARCH = "upc_code";
  private static final String ID2 = "ID";
  private static final byte[] DESCRIPTION = "DESCRIPTION".getBytes();
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final Double WIDTH = Double.valueOf(98.42);
  private static final Double WEIGHT = Double.valueOf(1.1);
  private static final Double HEIGHT = Double.valueOf(2.2);
  private static final String UOM = "uom";
  private static final String UNIQUE_SELLING_POINT = "unique-selling-point";
  private static final Double SHIPPING_WEIGHT = Double.valueOf(8.45);
  private static final Double LENGTH = Double.valueOf(5.12);
  private static final String BRAND_NAME = "brand-name";
  private static final String CATEGORY_ID = "category-id";
  private static final String CATEGORY_NAME = "category-name";
  private static final String PRODUCT_CODE = "product-code";
  private static final String WRONG_PRODUCT_CODE = "wrong-product-code";
  private static final String PRODUCT_CODE_SEARCH = "-code";
  private static final String ID = "id";
  private static final String NAME = "product-name";
  private static final String HIDDEN_ID = "hidden-id";
  private static final String HIDDEN_NAME = "hidden-product-name";
  private static final String NAME_SEARCH = "-name";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String CATEGORY_CODE = "CA-1000010";
  private static final String PRODUCT_CODE_1 = "MTA-0011708";
  private static final String CATALOG_NAME = "CATALOG_NAME";
  private static final String CATALOG_CODE = "CATALOG_CODE";
  private static final String LOCATION_PATH = "LOCATION_PATH";

  private static final String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";

  private static final String MASTER_CATEGORY = "MASTER_CATEGORY";
  private static final String SALES_CATEGORY = "SALES_CATEGORY";
  private static final String VALUE = "value";

  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;
  private static final int TOTAL_RECORDS = 1;
  private static final String SOURCE_ITEM_CODE = "sourceItemCode1";

  private Category category;
  private ProductAttribute productAttribute;
  private ProductCategory productCategory;
  private ProductImage productImage;
  private ProductItemImage productItemImage;
  private ProductItem productItem;
  private Product product;
  private AttributeRequest attributeRequest;
  private ProductRequest productRequest;
  private ProductItemRequest productItemRequest;
  private CategoryRequest categoryRequest;
  private AttributeValueDTO attributeValueDTO;
  private Page<AttributeValueDTO> attributeValueDTOPage;
  private List<AttributeValueDTO> attributeValueDTOS;
  private Pageable pageable;
  private List<ShippingResponse> shippingResponses;
  private List<ProductItemImage> productItemImages1;


  @InjectMocks
  private ConverterUtil converterUtil;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);

    Catalog catalog = new Catalog(CATALOG_NAME, CATALOG_CODE, CatalogType.MASTER_CATALOG, STORE_ID);
    List<CategoryAttribute> categoryAttributes = new ArrayList<CategoryAttribute>();
    Attribute definingAttribute =
        new Attribute(ATTRIBUTE_NAME, AttributeType.DEFINING_ATTRIBUTE, true, STORE_ID);
    Attribute preDefiningattribute =
        new Attribute(ATTRIBUTE_NAME, AttributeType.DEFINING_ATTRIBUTE, true, STORE_ID);
    CategoryAttribute categoryAttribute =
        new CategoryAttribute(this.category, definingAttribute, 1, true, false, STORE_ID);
    List<CategoryReference> masterCategoryReferences = new ArrayList<CategoryReference>();
    Category salesCategory = new Category(STORE_ID, SALES_CATEGORY, 1);
    Category masterCategory = new Category(STORE_ID, MASTER_CATEGORY, 1);
    masterCategory.setUmkm(true);
    CategoryReference CategoryReference = new CategoryReference(masterCategory, salesCategory);
    masterCategoryReferences.add(CategoryReference);
    // Category initialization
    this.category = new Category(STORE_ID, CATEGORY_NAME, 1);
    this.category.setCatalog(catalog);
    categoryAttributes.add(categoryAttribute);
    this.category.setCategoryAttributes(categoryAttributes);
    this.category.setMasterCategoryReferences(masterCategoryReferences);
    this.category.setSalesCategoryReferences(masterCategoryReferences);
    this.category.setUmkm(true);

    // ProductImage initialization
    this.productImage = new ProductImage();

    // ProductItemImage initialization
    this.productItemImage = new ProductItemImage();

    // ProductAttribute initialization
    AllowedAttributeValue allowedAttributeValue =
        new AllowedAttributeValue(definingAttribute, ATTRIBUTE_NAME, STORE_ID, 1);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        new PredefinedAllowedAttributeValue(preDefiningattribute, ATTRIBUTE_NAME, STORE_ID, 1);
    this.productAttribute = new ProductAttribute();
    this.productAttribute.setAttribute(definingAttribute);
    List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
    ProductAttributeValue allowedproductAttributeValue = new ProductAttributeValue();
    ProductAttributeValue predefineproductAttributeValue = new ProductAttributeValue();
    allowedproductAttributeValue
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    predefineproductAttributeValue
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    allowedproductAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
    predefineproductAttributeValue
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValues.add(allowedproductAttributeValue);
    productAttributeValues.add(predefineproductAttributeValue);
    this.productAttribute.setProductAttributeValues(productAttributeValues);

    // productItem initialization
    List<ProductItemImage> productItemImages = new ArrayList<ProductItemImage>();
    productItemImages.add(this.productItemImage);
    this.productItem = new ProductItem();
    this.productItem.setProductItemImages(productItemImages);

    // product initialization
    this.productCategory = new ProductCategory();
    this.productCategory.setCategory(this.category);
    // product initialization
    List<ProductImage> productImages = new ArrayList<ProductImage>();
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    List<ProductCategory> productCategories = new ArrayList<ProductCategory>();
    productImages.add(this.productImage);
    productItems.add(this.productItem);
    productAttributes.add(this.productAttribute);
    productCategories.add(this.productCategory);
    this.product = new Product();
    this.product.setProductImages(productImages);
    this.product.setProductItems(productItems);
    this.product.setProductAttributes(productAttributes);
    this.product.setProductCategories(productCategories);

    // AttributeRequest initialization
    this.attributeRequest = new AttributeRequest(ATTRIBUTE_NAME, ATTRIBUTE_CODE,
        com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE, true, true, STORE_ID);
    List<AllowedAttributeValueRequest> allowedAttributeValues =
        new ArrayList<AllowedAttributeValueRequest>();
    AllowedAttributeValueRequest AllowedAttributeValueRequest =
        new AllowedAttributeValueRequest(ATTRIBUTE_CODE, 1, STORE_ID);
    allowedAttributeValues.add(AllowedAttributeValueRequest);
    this.attributeRequest.setAllowedAttributeValues(allowedAttributeValues);

    // ProductItemRequest initialization
    this.productItemRequest = new ProductItemRequest();
    List<Image> images = new ArrayList<Image>();
    Image image = new Image(true, "  ", 0);
    image.setActive(true);
    images.add(image);
    List<ProductItemAttributeValueRequest> productItemAttributeValues = new ArrayList<ProductItemAttributeValueRequest>();
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(this.attributeRequest);
    productItemAttributeValues.add(productItemAttributeValueRequest );
    this.productItemRequest.setProductItemAttributeValues(productItemAttributeValues);
 
    this.productItemRequest.setImages(images);
        
 // categoryRequest initialization
    this.categoryRequest = new CategoryRequest();
    CatalogRequest catalogRequest = new CatalogRequest();
    this.categoryRequest.setCatalog(catalogRequest);
    CategoryRequest parentCategory = new CategoryRequest();
    this.categoryRequest.setParentCategory(parentCategory);
    
 // ProductRequest initialization
    List<ProductItemRequest>  ProductItemRequests = new ArrayList<>();
    List<ProductCategoryRequest> ProductCategoryRequests = new ArrayList<ProductCategoryRequest>();
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    List<ProductAttributeRequest> ProductAttributeRequests = new ArrayList<ProductAttributeRequest>();
    com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest ProductAttributeRequest = new ProductAttributeRequest();
    List<ProductAttributeValueRequest> ProductAttributeValueRequests = new ArrayList<ProductAttributeValueRequest>();
    ProductAttributeValueRequest ProductAttributeValueRequest = new ProductAttributeValueRequest();
    ProductAttributeValueRequest.setAllowedAttributeValue(AllowedAttributeValueRequest);
    ProductAttributeValueRequests.add(ProductAttributeValueRequest);
    ProductAttributeRequest.setProductAttributeValues(ProductAttributeValueRequests);
    ProductAttributeRequest.setAttribute(this.attributeRequest);
    ProductAttributeRequests.add(ProductAttributeRequest);
    productCategoryRequest.setCategory(this.categoryRequest);
    ProductCategoryRequests.add(productCategoryRequest);
    ProductItemRequests.add(this.productItemRequest);
    this.productRequest = new ProductRequest();
    this.productRequest.setProductItems(ProductItemRequests); 
    this.productRequest.setProductCategories(ProductCategoryRequests);
    this.productRequest.setProductAttributes(ProductAttributeRequests);

    // AttributeValueDTO page initialisation
    this.attributeValueDTO = new AttributeValueDTO();
    this.attributeValueDTO.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    this.attributeValueDTO.setValue(VALUE);
    this.attributeValueDTOS = new ArrayList<>();
    this.attributeValueDTOS.add(attributeValueDTO);
    this.pageable = PageRequest.of(PAGE_NUMBER, PAGE_SIZE);
    this.attributeValueDTOPage = new PageImpl<>(attributeValueDTOS, pageable, TOTAL_RECORDS);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(false);
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImage.setMainImages(true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setOriginalImage(true);
    productItemImage2.setLocationPath(LOCATION_PATH);
    productItemImage2.setMainImages(true);
    productItemImages1 = Arrays.asList(productItemImage, productItemImage1, productItemImage2);
  }

  @AfterEach
  public void postTest() {

  }

  @Test
  public void convertCategoryToCategoryDetailResponseTest() throws Exception {
    category.setParentCategory(new Category());
    category.setOriginalSalesCategory(new OriginalSalesCategory());
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(2).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(2).setMasterCategory(new Category());
    CategoryDetailResponse response =
        this.converterUtil.convertCategoryToCategoryDetailResponse(this.category,
            new HashSet<>());
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailResponseHalalCategoryTest() throws Exception {
    category.setParentCategory(new Category());
    category.setOriginalSalesCategory(new OriginalSalesCategory());
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(2).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(2).setMasterCategory(new Category());
    Category category1 = new Category();
    category1.setHalalCategory(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(3).setSalesCategory(category1);
    category.getAllSalesCategoryReferences().get(3).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(4).setSalesCategory(category1);
    category.getAllSalesCategoryReferences().get(4).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(4).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(4).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category1.setMarkForDelete(true);
    category.getAllSalesCategoryReferences().get(5).setSalesCategory(category1);
    category.getAllSalesCategoryReferences().get(5).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(5).setMarkForDelete(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(6).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(6).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(6).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(6).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(7).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(7).setMarkForDelete(true);
    category.getAllSalesCategoryReferences().get(7).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(7).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(7).setMasterCategory(new Category());
    Category category2 = new Category();
    category2.setHalalCategory(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(8).setSalesCategory(category2);
    category.getAllSalesCategoryReferences().get(8).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(8).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(8).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.SALES_CATALOG);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(9).setSalesCategory(category2);
    category.getAllSalesCategoryReferences().get(9).setMasterCategory(new Category());
    CategoryDetailResponse response =
        this.converterUtil.convertCategoryToCategoryDetailResponse(this.category,
            new HashSet<>());
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailResponseHalalCategoryTest2() throws Exception {
    category.setParentCategory(new Category());
    category.setOriginalSalesCategory(new OriginalSalesCategory());
    category.setMasterCategoryReferences(new ArrayList<>());
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    Category category1 = new Category();
    category1.setHalalCategory(true);
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(category1);
    CategoryDetailResponse response =
        this.converterUtil.convertCategoryToCategoryDetailResponse(this.category,
            new HashSet<>());
    assertNotNull(response);
    assertTrue(response.isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseTest() throws Exception {
    category.setParentCategory(new Category());
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
            new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseTest_excludeHideFromSellerTrue() throws Exception {
    category.setParentCategory(new Category());
    Attribute attribute = new Attribute();
    attribute.setHideForSeller(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    category.setCategoryAttributes(List.of(categoryAttribute));
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
            new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
    assertTrue(CollectionUtils.isEmpty(response.getCategoryAttributes()));
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseTestFilteredResponse_excludeHideFromSellerTrue()
      throws Exception {
    category.setParentCategory(new Category());
    Attribute attribute = new Attribute();
    attribute.setHideForSeller(true);
    attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    category.getCategoryAttributes().add(categoryAttribute);
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
            new HashSet<>(), shippingResponses, true);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
    assertEquals(2, response.getCategoryAttributes().size());
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseTest_excludeHideFromSellerFalse() throws Exception {
    category.setParentCategory(new Category());
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setHideForSeller(true);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setAttribute(attribute);
    category.getCategoryAttributes().add(categoryAttribute);
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
            new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
    assertTrue(CollectionUtils.isNotEmpty(response.getCategoryAttributes()));
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseValueTypesTest() throws Exception {
    List<String> valueTypes = List.of(VALUE);
    Attribute attribute =
        new Attribute(ATTRIBUTE_NAME, AttributeType.DEFINING_ATTRIBUTE, true, STORE_ID);
    attribute.setValueTypes(new ObjectMapper().writeValueAsString(valueTypes));
    CategoryAttribute categoryAttribute =
        new CategoryAttribute(this.category, attribute, 1, true, false, STORE_ID);
    List<CategoryAttribute> categoryAttributes = new ArrayList<>();
    categoryAttributes.add(categoryAttribute);
    category.setParentCategory(new Category());
    category.setCategoryAttributes(categoryAttributes);
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
            new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailAndSalesCategoryTest() throws Exception {
    category.setParentCategory(new Category());
    category.setCategoryAttributes(new ArrayList<>());
    category.getMasterCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
            .setCatalogType(CatalogType.SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setHalalCategory(false);
    CategoryDetailAndShippingResponse response =
            this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
                    new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailAndSalesCategoryFalseTest() throws Exception {
    category.setParentCategory(new Category());
    category.setCategoryAttributes(new ArrayList<>());
    category.getMasterCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
            .setCatalogType(CatalogType.SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setHalalCategory(true);
    CategoryDetailAndShippingResponse response =
            this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
                    new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }


  @Test
  public void convertCategoryToCategoryDetailAndSalesCategoryMarkForDeleteFalseTest() throws Exception {
    category.setParentCategory(new Category());
    category.setCategoryAttributes(new ArrayList<>());
    category.getMasterCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
            .setCatalogType(CatalogType.SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(true);
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setHalalCategory(true);
    CategoryDetailAndShippingResponse response =
            this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
                    new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseWithEmptyCategoryAttributeTest() throws Exception {
    category.setParentCategory(new Category());
    category.setCategoryAttributes(new ArrayList<>());
    category.getMasterCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(true);
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setHalalCategory(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(2).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(2).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setHalalCategory(false);
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category,
            new HashSet<>(), shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
  }

  @Test
  public void convertCategoryToCategoryDetailAndShippingResponseWithEmptyCategoryAttributeAndHalalCategoryTrueTest()
      throws Exception {
    category.setParentCategory(new Category());
    category.setCategoryAttributes(new ArrayList<>());
    category.getMasterCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setHalalCategory(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(2).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().getCatalog()
        .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(2).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setHalalCategory(false);
    CategoryDetailAndShippingResponse response =
        this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category, new HashSet<>(),
            shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
    assertTrue(response.getHalalSalesCategoryReferences().get(0).getHalalSalesCategoryReference().isHalalCategory());
  }

  @Test
  public void convertCategoryToCategoryDetailAndSalesCategoryNullTest()
          throws Exception {
    category.setParentCategory(new Category());
    category.setCategoryAttributes(new ArrayList<>());
    category.getMasterCategoryReferences().get(0).setMarkForDelete(false);
    category.getMasterCategoryReferences().get(0).getSalesCategory().setHalalCategory(false);
    category.getAllSalesCategoryReferences().get(0).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(1).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().getCatalog()
            .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(1).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(1).setMarkForDelete(false);
    category.getAllSalesCategoryReferences().get(1).getSalesCategory().setHalalCategory(true);
    category.getAllSalesCategoryReferences().add(new CategoryReference());
    category.getAllSalesCategoryReferences().get(2).setSalesCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setCatalog(new Catalog());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().getCatalog()
            .setCatalogType(CatalogType.B2B_SALES_CATALOG);
    category.getAllSalesCategoryReferences().get(2).setMasterCategory(new Category());
    category.getAllSalesCategoryReferences().get(2).getSalesCategory().setHalalCategory(false);
    CategoryDetailAndShippingResponse response =
            this.converterUtil.convertCategoryToCategoryDetailAndShippingResponse(this.category, new HashSet<>(),
                    shippingResponses, false);
    assertNotNull(response);
    assertTrue(response.isUmkm());
    assertTrue(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isUmkm());
    assertFalse(response.getMasterCategoryReferences().get(0).getMasterCategoryReference().isHalalCategory());
  }

  @Test
  public void convertProductAttributeToResponseTest() throws Exception {
    ProductAttributeResponse response =
        this.converterUtil.convertProductAttributeToResponse(this.productAttribute);
    assertNotNull(response);
  }


  @Test
  public void convertProductCategoryToResponseTest() throws Exception {
    ProductCategoryResponse response =
        this.converterUtil.convertProductCategoryToResponse(this.productCategory);
    assertNotNull(response);
    Assertions.assertEquals(CATALOG_NAME, response.getCategory().getCatalog().getName());
    Assertions.assertEquals(CatalogType.MASTER_CATALOG.name(), response.getCategory().getCatalog().getCatalogType());
  }

  @Test
  public void convertProductItemsToProductItemDetailResponseListTest() throws Exception {
    productItemImage.setMainImages(Boolean.TRUE);
    productItemImage.setMarkForDelete(Boolean.FALSE);
    productItem.setProductItemImages(Arrays.asList(productItemImage));
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    productItem.setProduct(product);
    List<ProductItemDetailResponse> responses =
        this.converterUtil.convertProductItemsToProductItemDetailResponseList(Arrays.asList(productItem));
    assertNotNull(responses);
    assertEquals(PRODUCT_CODE, responses.get(0).getProductResponse().getProductCode());
    assertEquals(ID, responses.get(0).getProductResponse().getId());
  }

  @Test
  public void convertProductItemsToProductItemDetailResponseListWithMfdTrueTest() throws Exception {
    productItemImage.setMainImages(Boolean.TRUE);
    productItemImage.setMarkForDelete(Boolean.TRUE);
    productItem.setProductItemImages(Arrays.asList(productItemImage));
    product.setId(ID);
    product.setProductCode(PRODUCT_CODE);
    productItem.setProduct(product);
    List<ProductItemDetailResponse> responses =
        this.converterUtil.convertProductItemsToProductItemDetailResponseList(Arrays.asList(productItem));
    assertNotNull(responses);
    assertEquals(PRODUCT_CODE, responses.get(0).getProductResponse().getProductCode());
    assertEquals(ID, responses.get(0).getProductResponse().getId());
  }

  @Test
  public void convertProductImageToResponseTest() throws Exception {
    Image response = this.converterUtil.convertProductImageToResponse(this.productImage);
    assertNotNull(response);
  }

  @Test
  public void convertProductItemImageToResponseTest() throws Exception {
    Image response = this.converterUtil.convertProductItemImageToResponse(this.productItemImage);
    assertNotNull(response);
  }

  @Test
  public void convertProductItemToResponseTest() throws Exception {
    initializeProductItem();
    productItem.setSourceItemCode(SOURCE_ITEM_CODE);
    productItem.setContentChanged(true);
    productItem.setVatApplicable(Boolean.TRUE);
    productItem.setNewlyAddedItem(Boolean.TRUE);
    ProductItemResponse response = this.converterUtil.convertProductItemToResponse(this.productItem, true);
    assertNotNull(response);
    assertEquals(3, response.getImages().size());
    assertEquals(SOURCE_ITEM_CODE, response.getSourceItemCode());
    assertTrue(response.isContentChanged());
    assertTrue(response.isNewlyAddedItem());
    assertTrue(response.getVatApplicable());
  }

  @Test
  public void convertProductItemToResponseOriginalImageTest() throws Exception {
    initializeProductItem();
    ProductItemResponse response = this.converterUtil.convertProductItemToResponse(this.productItem, false);
    assertNotNull(response);
    assertEquals(2, response.getImages().size());
    assertNull(response.getSourceItemCode());
    assertFalse(response.isContentChanged());
  }

  @Test
  public void convertProductItemToResponseOriginalImageTrueTest() throws Exception {
    initializeProductItem();
    ProductItemResponse response = this.converterUtil.convertProductItemToResponse(this.productItem, false);
    assertNotNull(response);
    assertEquals(2, response.getImages().size());
    assertNull(response.getSourceItemCode());
    assertFalse(response.isContentChanged());
  }

  private void initializeProductItem() {
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(true);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setOriginalImage(false);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setOriginalImage(null);
    productItemImage2.setActive(true);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setOriginalImage(null);
    productItemImage3.setActive(true);
    productItemImage.setMarkForDelete(true);
    productItem.setProductItemImages(new ArrayList<>(Arrays
        .asList(productItemImage, productItemImage1, productItemImage2)));
  }

  @Test
  public void convertProductToProductDetailResponseTest() throws Exception {
    initializeProduct();
    ProductDetailResponse response =
        this.converterUtil.convertProductToProductDetailResponse(this.product, false);
    assertNotNull(response);
    assertEquals(3, response.getImages().size());
  }

  @Test
  public void convertProductToProductDetailResponseOriginalImagesTrueTest() throws Exception {
    initializeProduct();
    ProductDetailResponse response =
        this.converterUtil.convertProductToProductDetailResponse(this.product, true);
    assertNotNull(response);
    assertEquals(4, response.getImages().size());
  }

  private void initializeProduct() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setOriginalImage(true);
    ProductImage productImage2 = new ProductImage();
    productImage2.setOriginalImage(false);
    ProductImage productImage3 = new ProductImage();
    productImage3.setOriginalImage(null);
    productImage3.setActive(false);
    ProductImage productImage4 = new ProductImage();
    productImage4.setOriginalImage(null);
    productImage4.setActive(true);
    product.setProductImages(new ArrayList<>(Arrays.asList(productImage1, productImage2, productImage3, productImage4)));
  }

  @Test
  public void convertAttributeValueDTOToAttributeValueResponse() throws Exception {
    GdnRestListResponse<AttributeValueResponse> response = this.converterUtil
        .convertAttributeValueDTOToAttributeValueResponse(this.attributeValueDTOPage, pageable, REQUEST_ID);
    assertNotNull(response);
  }

  @Test
  public void convertAttributeValueDTOToAttributeValueResponse_NullTest() throws Exception {
    GdnRestListResponse<AttributeValueResponse> response =
        ConverterUtil.convertAttributeValueDTOToAttributeValueResponse(null, pageable, REQUEST_ID);
    assertEquals(new ArrayList<>(), response.getContent());
  }

  @Test
  public void convertRequestToAttributeTest() throws Exception {
    Attribute response = this.converterUtil.convertRequestToAttribute(this.attributeRequest);
    assertNotNull(response);
  }

  @Test
  public void convertProductToMasterProductResponseTest() throws Exception {
    MasterProductResponse response = this.converterUtil.convertProductToMasterProductResponse(this.product);
    assertNotNull(response);
  }
  
  @Test
  public void convertRequestToProductTest() throws Exception {
    Product response = this.converterUtil.convertRequestToProduct(this.productRequest);
    assertNotNull(response);
  }
  
  @Test
  public void convertRequestToProductItemImageTest() throws Exception {
    List<ProductItemImage> response = this.converterUtil.convertRequestToProductItemImage(this.productRequest, this.productItemRequest, this.productItem);
    assertNotNull(response);
    assertTrue(response.get(0).isActive());
  }

  @Test
  public void convertRequestToProductItemImageWithEmptyItemImagesTest() throws Exception {
    this.productItemRequest.setImages(null);
    List<ProductItemImage> response = this.converterUtil.convertRequestToProductItemImage(this.productRequest, this.productItemRequest, this.productItem);
    assertNotNull(response);
  }

  @Test
  public void convertRequestToProductItemImageWithNoMainImagesTest() throws Exception {
    this.productItemRequest.getImages().get(0).setMainImages(false);;
    List<ProductItemImage> response = this.converterUtil.convertRequestToProductItemImage(
        this.productRequest, this.productItemRequest, this.productItem);
    assertNotNull(response);
    assertTrue(response.get(0).isActive());
  }

  @Test
  public void getPaginatedCategoryList_page0_size2() {
    //List: 1,2,3,4,5,6,7,8,9,10
    List<CategoryResponse> mockResponseList=getCategoryList();
    int pageNum=0;
    int size=2;
    List<CategoryResponse> paginatedCategoryList = this.converterUtil.getPaginatedCategoryList
        (mockResponseList, pageNum, size);
    assertNotNull(paginatedCategoryList);
    assertFalse(paginatedCategoryList.isEmpty());
    assertEquals(size,paginatedCategoryList.size());
    assertEquals("1",paginatedCategoryList.get(0).getId());
    assertEquals("2",paginatedCategoryList.get(1).getId());
  }
  @Test
  public void getPaginatedCategoryList_page2_size2() {
    List<CategoryResponse> mockResponseList=getCategoryList();
    //List: 1,2,3,4,5,6,7,8,9,10
    int pageNum=2;
    int size=2;
    List<CategoryResponse> paginatedCategoryList = this.converterUtil.getPaginatedCategoryList
        (mockResponseList, pageNum, size);
    assertNotNull(paginatedCategoryList);
    assertFalse(paginatedCategoryList.isEmpty());
    assertEquals(size,paginatedCategoryList.size());
    assertEquals("5",paginatedCategoryList.get(0).getId());
    assertEquals("6",paginatedCategoryList.get(1).getId());
  }
  @Test
  public void getPaginatedCategoryList_pageNumExceed_returnEmpty() {
    List<CategoryResponse> mockResponseList=getCategoryList();
    //List: 1,2,3,4,5,6,7,8,9,10
    int pageNum=10;
    int size=2;
    List<CategoryResponse> paginatedCategoryList = this.converterUtil.getPaginatedCategoryList
        (mockResponseList, pageNum, size);
    assertNotNull(paginatedCategoryList);
    assertTrue(paginatedCategoryList.isEmpty());
  }

  @Test
  public void getPaginatedCategoryList_lastPage_sizeExceed_returnTillLastElement() {
    List<CategoryResponse> mockResponseList=getCategoryList();
    //List: 1,2,3,4,5,6,7,8,9,10
    int pageNum=3;
    int size=3;
    List<CategoryResponse> paginatedCategoryList = this.converterUtil.getPaginatedCategoryList
        (mockResponseList, pageNum, size);
    assertNotNull(paginatedCategoryList);
    assertFalse(paginatedCategoryList.isEmpty());
    assertEquals(1,paginatedCategoryList.size());
    assertEquals("10",paginatedCategoryList.get(0).getId());
  }

  @Test
  public void getPaginatedCategoryList_lastPage_exactSize() {
    List<CategoryResponse> mockResponseList=getCategoryList();
    //List: 1,2,3,4,5,6,7,8,9,10
    int pageNum=4;
    int size=2;
    List<CategoryResponse> paginatedCategoryList = this.converterUtil.getPaginatedCategoryList
        (mockResponseList, pageNum, size);
    assertNotNull(paginatedCategoryList);
    assertFalse(paginatedCategoryList.isEmpty());
    assertEquals(size,paginatedCategoryList.size());
    assertEquals("9",paginatedCategoryList.get(0).getId());
    assertEquals("10",paginatedCategoryList.get(1).getId());
  }

  private List<CategoryResponse> getCategoryList() {
    List<CategoryResponse> categoryResponseList=new ArrayList<>();
    for(int i=1;i<11;i++){
      CategoryResponse response=new CategoryResponse();
      response.setId(String.valueOf(i));
      categoryResponseList.add(response);
    }
    return categoryResponseList;
  }

  @Test
  public void sortProductImagesBySequenceIdProductNullTest() {
    Product product = null;
    this.converterUtil.sortProductImagesBySequenceId(product);

  }

  @Test
  public void sortProductImagesBySequenceIdProductImageEmptyTest() {
    Product product = new Product();
    this.converterUtil.sortProductImagesBySequenceId(product);

  }

  @Test
  public void sortProductItemImagesBySequenceId_ProductItemNullTest() {
    ProductItem productItem = null;
    this.converterUtil.sortProductItemImagesBySequenceId(productItem);

  }

  @Test
  public void sortProductItemImagesBySequenceId_ProductItemImageEmptyTest() {
    ProductItem productItem = new ProductItem();
    this.converterUtil.sortProductItemImagesBySequenceId(productItem);

  }
  @Test
  public void convertToCategoryResponseTest(){

    List<Category> categoryList=new ArrayList<>();
    for(int i=1;i<11;i++){
      Category category=new Category();
      category.setId(String.valueOf(i));
      categoryList.add(category);
      List<CategoryResponse> response = converterUtil.convertToCategoryResponse(categoryList);
      Assertions.assertEquals(categoryList.size(), response.size());
      Assertions.assertEquals(categoryList.get(0).getId(), response.get(0).getId());

    }
  }

  @Test
  public void filterProductItemImagesOriginalImage() {
    List<ProductItemImage> productItemImages1 =
        this.converterUtil.filterProductItemImages(true, this.productItemImages1);
    Assertions.assertEquals(3, productItemImages1.size());
  }

  @Test
  public void filterProductItemImagesEditedImage() {
    productItemImages1.get(0).setOriginalImage(true);
    productItemImages1.get(0).setEdited(true);
    productItemImages1.get(0).setActive(false);
    List<ProductItemImage> productItemImages1 =
        this.converterUtil.filterProductItemImages(false, this.productItemImages1);
    Assertions.assertEquals(1, productItemImages1.size());
  }

  @Test
  public void filterProductItemImagesEdited1Image() {
    productItemImages1.get(0).setOriginalImage(true);
    productItemImages1.get(0).setEdited(true);
    productItemImages1.get(0).setActive(true);
    List<ProductItemImage> productItemImages1 =
        this.converterUtil.filterProductItemImages(false, this.productItemImages1);
    Assertions.assertEquals(2, productItemImages1.size());
  }

  @Test
  public void filterProductItemImagesOriginalImageFalse() {
    List<ProductItemImage> productItemImages1 =
        this.converterUtil.filterProductItemImages(false, this.productItemImages1);
    Assertions.assertEquals(2, productItemImages1.size());
  }

  @Test
  public void convertToProductItemDetailResponseTest() {
    productItem.setProduct(new Product());
    productItem.setProductItemImages(productItemImages1);
    List<ProductItemDetailResponse> productItemDetailResponses =
        this.converterUtil.convertToProductItemDetailResponse(Arrays.asList(productItem), true);
    Assertions.assertEquals(1, productItemDetailResponses.size());
    Assertions.assertEquals(2, productItemDetailResponses.get(0).getImages().size());
  }

  @Test
  public void convertToProductItemDetailResponseOriginalImageFalseTest() {
    productItem.setProduct(new Product());
    productItem.setProductItemImages(productItemImages1);
    List<ProductItemDetailResponse> productItemDetailResponses =
        this.converterUtil.convertToProductItemDetailResponse(Arrays.asList(productItem), false);
    Assertions.assertEquals(1, productItemDetailResponses.size());
    Assertions.assertEquals(1, productItemDetailResponses.get(0).getImages().size());
  }
}
