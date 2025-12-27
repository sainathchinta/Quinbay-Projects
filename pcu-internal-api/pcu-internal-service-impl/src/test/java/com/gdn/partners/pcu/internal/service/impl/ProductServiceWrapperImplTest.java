package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.internal.verification.VerificationModeFactory.times;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.CacheProductService;
import com.gdn.partners.pcu.internal.service.CategoryService;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * Created by govind on 16/01/2019 AD.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductServiceWrapperImplTest {

  private static final String BARCODE = "barcode";
  private static final String ID = "id";
  private static final String GENERATED_ITEM_NAME = "generated-item-name";
  private static final String GENERATED_ITEM_NAME_2 = "generated-item-name-2";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTR_VALUE = "attr-value";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String ALLOWED_ATTR_VALUE = "allowed-attr-value";
  private static final String USER_NAME = "username";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATALOG_CODE = "catalog-code";
  private static final String INTERNAL_ACTIVATION_PERIOD = "12";
  private static final String PRODUCT_CODE = "productCode";


  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE =
      "predefined-allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTR_VALUE = "predefined-allowed-attr-value";

  @Mock
  private ProductService productService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private CacheProductService cacheProductService;

  @InjectMocks
  private ProductServiceWrapperImpl productServiceWrapper;

  private ProductRequest productRequest;
  private ProductDetailResponse productDetailResponse;


  @BeforeEach
  public void init() {
    productRequest = new ProductRequest();
    productRequest.setName(PRODUCT_NAME);
    productRequest.setId(ID);
    productRequest.setUpdatedBy(USER_NAME);
    productRequest.setProductCode(PRODUCT_CODE);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setId(ID);
    productDetailResponse.setUpdatedBy(USER_NAME);


    Image imageRequest = new Image();
    imageRequest.setHashCode("hash-code");
    imageRequest.setLocationPath("location-path");
    List<Image> imageRequests = new ArrayList<>();
    imageRequests.add(imageRequest);
    productRequest.setImages(imageRequests);
    TreeMap<String, String> attributesMap = new TreeMap<String, String>();
    attributesMap.put("image1", "hash-code1");

    productDetailResponse.setImages(imageRequests);

    List<ProductCategoryRequest> productCategories = new ArrayList<>();
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest category = new CategoryRequest();
    category.setCategoryCode(CATEGORY_CODE);
    CatalogRequest catalog = new CatalogRequest();
    catalog.setCatalogCode(CATALOG_CODE);
    category.setCatalog(catalog);
    productCategoryRequest.setCategory(category);
    productCategories.add(productCategoryRequest);
    productRequest.setProductCategories(productCategories);

    List<ProductCategoryResponse> productCategoriesResponse = new ArrayList<>();
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogCode(CATALOG_CODE);
    categoryResponse.setCatalog(catalogResponse);
    productCategoryRequest.setCategory(category);
    productCategoryResponse.setCategory(categoryResponse);
    productCategoriesResponse.add(productCategoryResponse);
    productDetailResponse.setProductCategoryResponses(productCategoriesResponse);

    List<ProductAttributeRequest> productAttributes = new ArrayList<>();
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    List<AllowedAttributeValueRequest> allowedAttributeValues = new ArrayList<>();
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueRequest.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValues.add(allowedAttributeValueRequest);
    attributeRequest.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValues = new ArrayList<>();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueRequest);
    attributeRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setSequence(2);
    productAttributes.add(productAttributeRequest);
    productRequest.setProductAttributes(productAttributes);


    List<ProductAttributeResponse> productAttributeResponses = new ArrayList<>();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses = new ArrayList<>();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse);
    attributeResponse.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueResponses);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.setSequence(2);
    productAttributeResponses.add(productAttributeResponse);
    productDetailResponse.setProductAttributeResponses(productAttributeResponses);

    List<ProductItemRequest> productItems = new ArrayList<>();
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemRequest.setImages(imageRequests);
    productItemRequest.setAttributesMap(attributesMap);
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(ATTR_VALUE);
    productItemAttributeValueRequest.setId("ID");
    List<ProductItemAttributeValueRequest> productItemAttributeValueRequests = new ArrayList<>();
    productItemAttributeValueRequests.add(productItemAttributeValueRequest);
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueRequests);
    productItems.add(productItemRequest);
    productRequest.setProductItems(productItems);

    Set<ProductItemResponse> productItemResponses = new HashSet<>();
    ProductItemResponse productItemReseponse = new ProductItemResponse();
    productItemReseponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemReseponse.setImages(imageRequests);
    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTR_VALUE);
    productItemAttributeValueResponse.setId("ID");
    List<ProductItemAttributeValueResponse> productItemAttributeValueResponses = new ArrayList<>();
    productItemAttributeValueResponses.add(productItemAttributeValueResponse);
    productItemReseponse.setProductItemAttributeValueResponses(productItemAttributeValueResponses);
    productItemResponses.add(productItemReseponse);
    productDetailResponse.setProductItemResponses(productItemResponses);

  }

  @Test
  public void updateProductTest() throws Exception{
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    Mockito.when(categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    productServiceWrapper
        .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_INTERNAL, productRequest, true, false);
    Mockito.verify(productService).updateProductAndPublishToPDT(productRequest);
    Mockito.verify(productService).findProduct(productRequest.getId());
    Mockito.verify(categoryService).findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.cacheProductService)
        .removeCurrentUserFromProductView(productRequest.getProductCode(),
            productRequest.getUpdatedBy());
    Mockito.verify(productService).saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  @Test
  public void updateProduct_whenInternalFlow3AddProductFalseTest() throws Exception{
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    Mockito.when(categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    productServiceWrapper
        .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_INTERNAL, productRequest, false, false);
    Mockito.verify(productService).updateProduct(productRequest, false);
    Mockito.verify(productService).findProduct(productRequest.getId());
    Mockito.verify(categoryService).findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.cacheProductService)
        .removeCurrentUserFromProductView(productRequest.getProductCode(),
            productRequest.getUpdatedBy());
    Mockito.verify(productService).saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  @Test
  public void updateProduct_whenItemsNotEqualsAndInternalFlow3AddProductFalseTest() throws Exception {
    productDetailResponse.getProductItemResponses().add(createProductItemResponse());
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    Mockito.when(productService.generateBarcode()).thenReturn(BARCODE);
    Mockito.when(categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    productServiceWrapper
        .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_INTERNAL, productRequest, false, false);
    Mockito.verify(productService).findProduct(productRequest.getId());
    Mockito.verify(productService, times(2)).updateProduct(productRequest, false);
    Mockito.verify(productService, times(2)).generateBarcode();
    Mockito.verify(categoryService).findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.cacheProductService)
        .removeCurrentUserFromProductView(productRequest.getProductCode(), productRequest.getUpdatedBy());
    Mockito.verify(productService).saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  private ProductItemResponse createProductItemResponse(){
    ProductItemResponse productItemReseponse = new ProductItemResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    productItemReseponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemReseponse.setImages(productDetailResponse.getImages());
    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue(ATTR_VALUE);
    productItemAttributeValueResponse.setId("ID");
    List<ProductItemAttributeValueResponse> productItemAttributeValueResponses = new ArrayList<>();
    productItemAttributeValueResponses.add(productItemAttributeValueResponse);
    productItemReseponse.setProductItemAttributeValueResponses(productItemAttributeValueResponses);
    return productItemReseponse;
  }

  @Test
  public void updateProduct_whenItemsNotEqualsTest() throws Exception{

    productDetailResponse.getProductItemResponses().add(createProductItemResponse());
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    Mockito.when(productService.generateBarcode()).thenReturn(BARCODE);
    Mockito.when(categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    productServiceWrapper
        .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_INTERNAL, productRequest, true, false);
    Mockito.verify(productService, times(2)).updateProductAndPublishToPDT(productRequest);
    Mockito.verify(productService).findProduct(productRequest.getId());
    Mockito.verify(productService, times(2)).generateBarcode();
    Mockito.verify(categoryService).findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.cacheProductService)
        .removeCurrentUserFromProductView(productRequest.getProductCode(),
            productRequest.getUpdatedBy());
    Mockito.verify(productService).saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  @Test
  public void updateProductForReviewTest() throws Exception{
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    Mockito.when(categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    productServiceWrapper
        .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_INTERNAL, productRequest, true, false);
    Mockito.verify(productService).updateProductAndPublishToPDT(productRequest);
    Mockito.verify(productService).findProduct(productRequest.getId());
    Mockito.verify(categoryService).findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.cacheProductService)
        .removeCurrentUserFromProductView(productRequest.getProductCode(),
            productRequest.getUpdatedBy());
    Mockito.verify(productService).saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  @Test
  public void updateProductForReview_whenItemsNotEqualsTest() throws Exception{
    productDetailResponse.getProductItemResponses().add(createProductItemResponse());
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    Mockito.when(productService.generateBarcode()).thenReturn(BARCODE);
    Mockito.when(categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    productServiceWrapper
        .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_INTERNAL, productRequest, true, false);
    Mockito.verify(productService, times(2)).updateProductAndPublishToPDT(productRequest);
    Mockito.verify(productService).findProduct(productRequest.getId());
    Mockito.verify(productService, times(2)).generateBarcode();
    Mockito.verify(categoryService).findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.cacheProductService)
        .removeCurrentUserFromProductView(productRequest.getProductCode(),
            productRequest.getUpdatedBy());
    Mockito.verify(productService).saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  @Test
  public void updateProduct_whenApplicationExceptionTest() throws Exception{
    boolean isValidResponse = true;
    String response = null;
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(null);
    try {
      response = this.productServiceWrapper
          .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_EXTERNAL, productRequest, false, false);
    } catch (ApplicationRuntimeException ex) {
      isValidResponse = false;
    }
    finally {
      Mockito.verify(productService).updateProduct(productRequest, false);
      Mockito.verify(productService).findProduct(productRequest.getId());
      Assertions.assertFalse(isValidResponse);
      Assertions.assertNull(response);
    }
  }

  @Test
  public void updateProduct_whenItemResponsesEmptyTest() throws Exception{
    boolean isValidResponse = true;
    String response = null;
    productDetailResponse.setProductItemResponses(null);
    Mockito.when(productService.findProduct(productRequest.getId())).thenReturn(productDetailResponse);
    try {
      response = this.productServiceWrapper
          .updateProduct(Constants.REQUEST_ID, Constants.USER_TYPE_EXTERNAL, productRequest, false, false);
    } catch (ApplicationRuntimeException ex) {
      isValidResponse = false;
    }
    finally {
      Mockito.verify(productService).updateProduct(productRequest, false);
      Mockito.verify(productService).findProduct(productRequest.getId());
      Assertions.assertFalse(isValidResponse);
      Assertions.assertNull(response);
    }
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.categoryService);
    verifyNoMoreInteractions(this.cacheProductService);
    verifyNoMoreInteractions(this.pbpFeign);
  }
}
