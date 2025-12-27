package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import static org.mockito.Mockito.verify;

public class TrackerServiceImplTest {

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<ProductCreationFailureDomainEventModel>
      productCreationFailureDomainEventModelCaptor;

  @InjectMocks
  private TrackerServiceImpl trackerService;

  private static final String REQUEST_ID = "requestId";
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final byte[] DESCRIPTION = "DESCRIPTION".getBytes();
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final Double WIDTH = 100.00;
  private static final Double WEIGHT = 20.00;
  private static final Double HEIGHT = 10.00;
  private static final String UOM = "uom";
  private static final String UNIQUE_SELLING_POINT = "unique-selling-point";
  private static final Double SHIPPING_WEIGHT = 20.00;
  private static final Double LENGTH = 15.5;
  private static final String BRAND_NAME = "brand-name";
  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_ITEM_NAME = "PRODUCT_ITEM_NAME";
  private static final String PRODUCT_ITEM_NAME_2 = "PRODUCT_ITEM_NAME_2";
  private static final String SKU_CODE_2 = "SKU_CODE_2";
  private static final String UPC_CODE_2 = "UPC_CODE_2";
  private static final String SKU_CODE_1 = "SKU_CODE_1";
  private static final String SOURCE_ITEM_CODE1 = "SOURCE_SKU_CODE_1";
  private static final String UPC_CODE_1 = "UPC_CODE_1";
  private static final String LOCATION_PATH = "LOCATION_PATH";
  private static final String CATEGORY_ID = "categoryId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String VALUE = "value";
  private static final String MERAH = "Merah";
  private static final String BIRU = "Biru";
  private static final String KUNING = "Kuning";
  private static final String GB_16 = "16 GB";
  private static final String GB_32 = "32 GB";
  private static final String WARNA = "Warna";
  private static final String WARNA_CODE = "Kode Warna";
  private static final String UKURAN = "Ukuran";
  private static final String UKURAN_CODE = "Kode Ukuran";
  private static final String BRAND = "Brand";
  private static final String DESCRIPTIVE_ATTRIBUTE = "descriptive";
  private static final String DESCRIPTIVE_ATTRIBUTE_CODE = "descriptiveCode";
  private static final String BRAND_CODE = "Kode Ukuran";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefinedAllowedAttributeCode";
  private static final String ID = "id";
  private static final String FLOW_TYPE = "flowType";

  private ProductCreationRequest productRequest;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    productRequest = new ProductCreationRequest();
    productRequest = new ProductCreationRequest();
    productRequest.setProductCode(PRODUCT_CODE);
    productRequest.setName(PRODUCT_NAME);
    productRequest.setLength(LENGTH);
    productRequest.setWidth(WIDTH);
    productRequest.setHeight(HEIGHT);
    productRequest.setWeight(WEIGHT);
    productRequest.setShippingWeight(SHIPPING_WEIGHT);
    productRequest.setDescription(DESCRIPTION);
    productRequest.setBrand(BRAND_NAME);
    productRequest.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    productRequest.setUom(UOM);
    productRequest.setStoreId(STORE_ID);
    productRequest.setPromoSKU(false);
    List<AllowedAttributeValueRequest> allowedAttributeWarna = new ArrayList<>();
    allowedAttributeWarna
        .add(new AllowedAttributeValueRequest(MERAH, 1, STORE_ID));
    allowedAttributeWarna.add(new AllowedAttributeValueRequest(BIRU, 2, STORE_ID));
    allowedAttributeWarna
        .add(new AllowedAttributeValueRequest(KUNING, 3, STORE_ID));

    List<AllowedAttributeValueRequest> allowedAttributeUkuran = new ArrayList<>();
    allowedAttributeUkuran
        .add(new AllowedAttributeValueRequest(GB_16, 1, STORE_ID));
    allowedAttributeUkuran
        .add(new AllowedAttributeValueRequest(GB_32, 1, STORE_ID));
    AttributeRequest warna =
        new AttributeRequest(WARNA, WARNA_CODE, AttributeType.DEFINING_ATTRIBUTE, true, true,
            STORE_ID);
    warna.setAllowedAttributeValues(allowedAttributeWarna);

    AttributeRequest ukuran =
        new AttributeRequest(UKURAN, UKURAN_CODE, AttributeType.DEFINING_ATTRIBUTE, true, true,
            STORE_ID);
    ukuran.setAllowedAttributeValues(allowedAttributeUkuran);
    List<ProductAttributeRequest> productAttributes = new ArrayList<>();
    ProductAttributeRequest productAttributeWarna =
        new ProductAttributeRequest(warna, WARNA, false, 1, STORE_ID);
    productAttributeWarna.getProductAttributeValues().add(
        new ProductAttributeValueRequest(allowedAttributeWarna.get(0), null, DescriptiveAttributeValueType.NONE,
            STORE_ID));
    productAttributes.add(productAttributeWarna);
    ProductAttributeRequest productAttributeUkuran =
        new ProductAttributeRequest(ukuran, UKURAN, false, 2, STORE_ID);
    productAttributeUkuran.getProductAttributeValues().add(
        new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.NONE,
            STORE_ID));
    productAttributes.add(productAttributeUkuran);

    AttributeRequest brand =
        new AttributeRequest(BRAND, BRAND_CODE, AttributeType.PREDEFINED_ATTRIBUTE, true, true, STORE_ID);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValue.setValue(VALUE);
    brand.setPredefinedAllowedAttributeValues(Collections.singletonList(predefinedAllowedAttributeValue));
    ProductAttributeRequest productAttributeBrand =
        new ProductAttributeRequest(ukuran, BRAND, false, 2, STORE_ID);
    productAttributeBrand.getProductAttributeValues().add(
        new ProductAttributeValueRequest(allowedAttributeUkuran.get(0), null, DescriptiveAttributeValueType.PREDEFINED, STORE_ID));
    productAttributeBrand.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeBrand.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().setId(ID);
    productAttributes.add(productAttributeBrand);

    AttributeRequest descriptiveAttribute =
        new AttributeRequest(DESCRIPTIVE_ATTRIBUTE, DESCRIPTIVE_ATTRIBUTE_CODE,
            AttributeType.DESCRIPTIVE_ATTRIBUTE, true, true, STORE_ID);
    ProductAttributeRequest productAttributeDescriptive =
        new ProductAttributeRequest(descriptiveAttribute, DESCRIPTIVE_ATTRIBUTE, false, 1, STORE_ID);
    productAttributeDescriptive.getProductAttributeValues().add(
        new ProductAttributeValueRequest(null, VALUE, DescriptiveAttributeValueType.NONE, STORE_ID));
    productAttributes.add(productAttributeDescriptive);

    productRequest.setProductAttributes(productAttributes);
    List<ProductItemRequest> productItemsRequest = new ArrayList<>();
    ProductItemRequest itemRequest1 =
        new ProductItemRequest(PRODUCT_ITEM_NAME, UPC_CODE_1,
            SKU_CODE_1, false, false, STORE_ID);
    List<ProductItemAttributeValueRequest> itemAttributeValueRequests = new ArrayList<>();
    itemAttributeValueRequests.add(new ProductItemAttributeValueRequest(new AttributeRequest(), VALUE, STORE_ID));
    itemRequest1.setId("1");
    itemRequest1.setProductItemAttributeValues(itemAttributeValueRequests);
    itemRequest1.setSourceItemCode(SOURCE_ITEM_CODE1);
    itemRequest1.setContentChanged(true);
    List<Image> imageList1 = new ArrayList<>();
    Image image1_1 = new Image();
    image1_1.setLocationPath(LOCATION_PATH);
    imageList1.add(image1_1);
    itemRequest1.setImages(imageList1);
    ProductItemRequest itemRequest2 =
        new ProductItemRequest(PRODUCT_ITEM_NAME_2, UPC_CODE_2,
            SKU_CODE_2, false, false, STORE_ID);
    itemRequest2.setId("2");
    itemRequest2.setProductItemAttributeValues(itemAttributeValueRequests);
    productItemsRequest.add(itemRequest1);
    productItemsRequest.add(itemRequest2);

    productRequest.setProductItems(productItemsRequest);
    List<ProductCategoryRequest> productCategoryRequests = new ArrayList<>();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setId(CATEGORY_ID);
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryRequests.add(new ProductCategoryRequest(categoryRequest, STORE_ID));
    productRequest.setProductCategories(productCategoryRequests);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void trackProductCreationFailureTest() {
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, productRequest, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }

  @Test
  public void trackProductCreationFailureTestNullProductRequest() {
    trackerService.trackProductCreationFailure(REQUEST_ID, null, null, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }

  @Test
  public void trackProductCreationFailureTestNoCategoriesNoAttributesNoItemsProductRequest() {
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, new ProductCreationRequest(), ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }

  @Test
  public void trackProductCreationFailureTestNoCategoryWithProductCategoriesProductRequest() {
    productRequest.getProductCategories().forEach(productCategoryRequest -> productCategoryRequest.setCategory(null));
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, productRequest, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }

  @Test
  public void trackProductCreationFailureTestNoAtributeWithProductAttributesProductRequest() {
    productRequest.getProductAttributes().forEach(productAttributeRequest -> productAttributeRequest.setAttribute(null));
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, productRequest, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }


  @Test
  public void trackProductCreationFailureTestNoProductAtributeValuesRequestsProductRequest() {
    productRequest.getProductAttributes()
        .forEach(productAttributeRequest -> productAttributeRequest.setProductAttributeValues(null));
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, productRequest, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }

  @Test
  public void trackProductCreationFailureTestNoAttributeInProductItemAttributesProductRequest() {
    productRequest.getProductItems().stream().flatMap(productItemRequest -> productItemRequest.getProductItemAttributeValues().stream())
        .forEach(productItemAttributeValueRequest -> productItemAttributeValueRequest.setAttribute(null));
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, productRequest, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }

  @Test
  public void trackProductCreationFailureTestNoProductItemAttributeValuesRequest() {
    productRequest.getProductItems().forEach(productItemRequest -> productItemRequest.setProductItemAttributeValues(null));
    trackerService.trackProductCreationFailure(REQUEST_ID, FLOW_TYPE, productRequest, ERROR_MESSAGE);
    verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_CREATION_FAILURE),
        productCreationFailureDomainEventModelCaptor.capture());
  }
}