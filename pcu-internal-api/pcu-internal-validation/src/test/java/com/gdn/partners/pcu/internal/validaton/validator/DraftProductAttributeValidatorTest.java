package com.gdn.partners.pcu.internal.validaton.validator;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import jakarta.validation.ConstraintValidatorContext;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import com.gdn.partners.pcu.internal.web.model.request.AllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CatalogWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;

public class DraftProductAttributeValidatorTest {

  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String GENERATED_ITEM_NAME = "generated-item-name";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTR_VALUE = "attr-value";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String ALLOWED_ATTR_VALUE = "allowed-attr-value";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined-allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTR_VALUE = "predefined-allowed-attr-value";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATALOG_CODE = "catalog-code";

  private ProductWebRequest productWebRequest;

  private DraftProductAttributeValidator draftProductAttributeValidator = new DraftProductAttributeValidator();

  @Mock
  private ConstraintValidatorContext constraintValidatorContext;

  @BeforeEach
  public void setUp() throws Exception {
    productWebRequest = new ProductWebRequest();
    productWebRequest = new ProductWebRequest();
    productWebRequest.setName(PRODUCT_NAME);
    productWebRequest.setProductCode(PRODUCT_CODE);
    productWebRequest.setVersion(2l);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setHashCode("hash-code");
    imageRequest.setLocationPath("location-path");
    List<ImageRequest> imageRequests = new ArrayList<>();
    imageRequests.add(imageRequest);
    productWebRequest.setImages(imageRequests);
    TreeMap<String, String> attributesMap = new TreeMap<String, String>();
    attributesMap.put("image1", "hash-code1");

    List<ProductCategoryWebRequest> productCategories = new ArrayList<>();
    ProductCategoryWebRequest productCategoryWebRequest = new ProductCategoryWebRequest();
    productCategoryWebRequest.setCategoryCode(CATEGORY_CODE);
    CatalogWebRequest catalogWebRequest = new CatalogWebRequest();
    catalogWebRequest.setCatalogCode(CATALOG_CODE);
    productCategoryWebRequest.setCatalog(catalogWebRequest);
    productCategories.add(productCategoryWebRequest);
    productWebRequest.setProductCategories(productCategories);

    List<ProductAttributeWebRequest> productAttributes = new ArrayList<>();
    ProductAttributeWebRequest productAttributeWebRequest = new ProductAttributeWebRequest();
    AttributeWebRequest attributeWebRequest = new AttributeWebRequest();
    attributeWebRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeWebRequest.setAttributeType(AttributeTypeWeb.DEFINING_ATTRIBUTE);
    List<AllowedAttributeValueWebRequest> allowedAttributeValues = new ArrayList<>();
    AllowedAttributeValueWebRequest allowedAttributeValueWebRequest = new AllowedAttributeValueWebRequest();
    allowedAttributeValueWebRequest.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueWebRequest.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValues.add(allowedAttributeValueWebRequest);
    attributeWebRequest.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValues = new ArrayList<>();
    PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValueWebRequest =
        new PredefinedAllowedAttributeValueWebRequest();
    predefinedAllowedAttributeValueWebRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueWebRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueWebRequest);
    attributeWebRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productAttributeWebRequest.setAttribute(attributeWebRequest);
    productAttributeWebRequest.setSequence(2);
    productAttributes.add(productAttributeWebRequest);
    productWebRequest.setProductAttributes(productAttributes);

    List<ProductItemWebRequest> productItems = new ArrayList<>();
    ProductItemWebRequest productItemRequest = new ProductItemWebRequest();
    productItemRequest.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemRequest.setImages(imageRequests);
    productItemRequest.setAttributesMap(attributesMap);
    ProductItemAttributeValueWebRequest productItemAttributeValueWebRequest = new ProductItemAttributeValueWebRequest();
    productItemAttributeValueWebRequest.setAttribute(attributeWebRequest);
    productItemAttributeValueWebRequest.setValue(ATTR_VALUE);
    productItemAttributeValueWebRequest.setId("ID");
    List<ProductItemAttributeValueWebRequest> productItemAttributeValueWebRequests = new ArrayList<>();
    productItemAttributeValueWebRequests.add(productItemAttributeValueWebRequest);
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueWebRequests);
    productItems.add(productItemRequest);
    productWebRequest.setProductItems(productItems);
  }


  @Test
  public void isValidTest() {
    boolean value = draftProductAttributeValidator.isValid(productWebRequest,constraintValidatorContext);
    Assertions.assertTrue(value);
  }

}