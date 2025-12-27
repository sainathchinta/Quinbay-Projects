package com.gdn.mta.product.util.validator;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;

public class DescriptiveFieldCharacterValidatorTest {
  private static String PRODUCT_NAME_NON_ASCII = "product_nameÀ";
  private static String PRODUCT_NAME_ASCII = "product_name";
  private static String PRODUCT_DESCRIPTION_NON_ASCII = "product_description_À";
  private static String PRODUCT_DESCRIPTION_ASCII = "product_description";
  private static String PRODUCT_USP_NON_ASCII = "product_usp_À";
  private static String PRODUCT_USP_ASCII = "product_usp";
  private static String PRODUCT_ATTRIBUTE_NON_ASCII = "product_attribute_À";
  private static String PRODUCT_ATTRIBUTE_ASCII = "product_attribute";


  @BeforeEach
  public void setUp() {

  }

  @Test
  public void validateProductCreationRequestProductNameTest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_NON_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_ASCII.getBytes(StandardCharsets.UTF_8));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), true);
    });
  }

  @Test
  public void validateProductCreationRequestDescriptionTest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_NON_ASCII.getBytes(StandardCharsets.UTF_8));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), true);
    });
  }

  @Test
  public void validateProductCreationRequestUSPTest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_ASCII.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setUniqueSellingPoint(PRODUCT_USP_NON_ASCII);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), true);
    });
  }

  @Test
  public void validateProductCreationRequestAttributeTest() {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(new AttributeRequest());
    productAttributeRequest2.setAttribute(attributeRequest);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_ASCII);
    productAttributeValueRequest3.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_NON_ASCII);
    productAttributeRequest2.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_ASCII.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setUniqueSellingPoint(PRODUCT_USP_ASCII);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1, productAttributeRequest2));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), true);
    });
  }

  @Test
  public void validateProductCreationRequestNullUspTest() {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(new AttributeRequest());
    productAttributeRequest2.setAttribute(attributeRequest);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_ASCII);
    productAttributeValueRequest3.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_NON_ASCII);
    productAttributeRequest2.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_ASCII.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1, productAttributeRequest2));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), true);
    });

  }

  @Test
  public void validateProductCreationRequestEmptyAttributeTest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_ASCII.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setProductAttributes(new ArrayList<>());
    DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), true);

  }

  @Test
  public void validateProductCreationRequestAttributeSwitchOffTest() {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(new AttributeRequest());
    productAttributeRequest2.setAttribute(attributeRequest);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_ASCII);
    productAttributeValueRequest3.setDescriptiveAttributeValue(PRODUCT_ATTRIBUTE_NON_ASCII);
    productAttributeRequest2.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setName(PRODUCT_NAME_ASCII);
    productCreationRequest.setDescription(PRODUCT_DESCRIPTION_ASCII.getBytes(StandardCharsets.UTF_8));
    productCreationRequest.setUniqueSellingPoint(PRODUCT_USP_ASCII);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest1, productAttributeRequest2));
    DescriptiveFieldCharacterValidator.validateProductCreationRequest(productCreationRequest, new ArrayList<>(), false);

  }

  @Test
  public void validateProductLevel3RequestProductNameTest() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_NON_ASCII);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, new ArrayList<>(),true);
    });
  }

  @Test
  public void validateProductLevel3RequestProductDescriptionTest() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_ASCII);
    productL3UpdateRequest.setDescription(PRODUCT_DESCRIPTION_NON_ASCII);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, new ArrayList<>(),true);
    });
  }

  @Test
  public void validateProductLevel3RequestProductUSPTest() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_ASCII);
    productL3UpdateRequest.setDescription(PRODUCT_DESCRIPTION_ASCII);
    productL3UpdateRequest.setUniqueSellingPoint(PRODUCT_USP_NON_ASCII);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, new ArrayList<>(),true);
    });
  }

  @Test
  public void validateProductLevel3RequestProductAttributeTest() {
    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    ProductLevel3AttributeRequest productLevel3AttributeRequest2 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductLevel3AttributeRequest productLevel3AttributeRequest3 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3AttributeRequest3.setValues(Arrays.asList("", PRODUCT_ATTRIBUTE_ASCII, PRODUCT_ATTRIBUTE_NON_ASCII));
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_ASCII);
    productL3UpdateRequest.setDescription(PRODUCT_DESCRIPTION_ASCII);
    productL3UpdateRequest.setUniqueSellingPoint(PRODUCT_USP_ASCII);
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1, productLevel3AttributeRequest2, productLevel3AttributeRequest3));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, new ArrayList<>(),true);
    });
  }

  @Test
  public void validateProductLevel3RequestNullUSPTest() {
    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    ProductLevel3AttributeRequest productLevel3AttributeRequest2 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductLevel3AttributeRequest productLevel3AttributeRequest3 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3AttributeRequest3.setValues(Arrays.asList("", PRODUCT_ATTRIBUTE_ASCII, PRODUCT_ATTRIBUTE_NON_ASCII));
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_ASCII);
    productL3UpdateRequest.setDescription(PRODUCT_DESCRIPTION_ASCII);
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1, productLevel3AttributeRequest2, productLevel3AttributeRequest3));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, new ArrayList<>(),true);
    });
  }

  @Test
  public void validateProductLevel3RequestEmptyProductAttributeTest() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_ASCII);
    productL3UpdateRequest.setDescription(PRODUCT_DESCRIPTION_ASCII);
    productL3UpdateRequest.setAttributes(new ArrayList<>());

    DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, Arrays.asList("."),true);
  }


  @Test
  public void validateProductLevel3RequestProductAttributeSwitchOffTest() {
    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    ProductLevel3AttributeRequest productLevel3AttributeRequest2 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductLevel3AttributeRequest productLevel3AttributeRequest3 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3AttributeRequest3.setValues(Arrays.asList("", PRODUCT_ATTRIBUTE_ASCII, PRODUCT_ATTRIBUTE_NON_ASCII));
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_ASCII);
    productL3UpdateRequest.setDescription(PRODUCT_DESCRIPTION_ASCII);
    productL3UpdateRequest.setUniqueSellingPoint(PRODUCT_USP_ASCII);
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1, productLevel3AttributeRequest2, productLevel3AttributeRequest3));

    DescriptiveFieldCharacterValidator.validateProductLevel3Request(productL3UpdateRequest, new ArrayList<>(),false);
  }
}
