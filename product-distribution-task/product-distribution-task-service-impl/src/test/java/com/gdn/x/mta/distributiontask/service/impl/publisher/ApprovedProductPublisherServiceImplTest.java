package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTEditedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTItemNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductApprovalStatusPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.dto.AttributeType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

/**
 * Created by Alok on 10/13/16.
 */
@ExtendWith(MockitoExtension.class)
public class ApprovedProductPublisherServiceImplTest {

  @InjectMocks
  private ApprovedProductPublisherServiceImpl approvedProductPublisherService;

  private ProductApprovalDetailStatus productApprovalDetailStatus;

  @Mock
  private ProductApprovalStatusPublisherService productApprovalStatusPublisherService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  ObjectMapper mapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String PRODUCT_CODE = "MTA-0001";
  private static final String ATTR_CODE_1 = "attribute_code_1";
  private static final String ATTR_CODE_2 = "attribute_code_2";
  private static final String ATTR_VALUE_1 = "value";
  private static final String SKU_CODE = "skuCode";
  private static final String BRAND_STATUS = "brandStatus";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String UPDATED_BY = "updatedBy";


  private Product createProduct() {
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    ProductItemImage productItemMainImage = new ProductItemImage();
    productItemMainImage.setMainImage(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImage(false);
    productItemImageList.add(productItemImage);
    productItemImageList.add(productItemMainImage);
    List<ProductItem> productItemList = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode(SKU_CODE);
    productItem.setProductItemImages(productItemImageList);
    productItemList.add(productItem);
    List<ProductAttribute> productAttributeList = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeCode(ATTR_CODE_1);
    productAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    ProductAttribute predifineProductAttribute = new ProductAttribute();
    predifineProductAttribute.setAttributeCode(ATTR_CODE_1);
    predifineProductAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    ProductAttribute descriptiveProductAttribute = new ProductAttribute();
    descriptiveProductAttribute.setAttributeCode(ATTR_CODE_1);
    descriptiveProductAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttributeList.add(productAttribute);
    productAttributeList.add(predifineProductAttribute);
    productAttributeList.add(descriptiveProductAttribute);
    List<ProductImage> productImageList = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImage.setMainImage(true);
    productImageList.add(productImage);
    Product product = new Product.Builder().productCode(PRODUCT_CODE).productItems(productItemList)
        .productAttributes(productAttributeList).productImages(productImageList).postLive(Boolean.TRUE).build();
    return product;
  }

  private Product createProductWithOriginalImageFlag() {
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    ProductItemImage productItemMainImage = new ProductItemImage();
    productItemMainImage.setMainImage(true);
    productItemMainImage.setOriginalImage(Boolean.FALSE);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImage(false);
    productItemImage.setOriginalImage(Boolean.FALSE);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setMainImage(false);
    productItemImage1.setOriginalImage(Boolean.TRUE);
    productItemImageList.add(productItemImage);
    productItemImageList.add(productItemMainImage);
    productItemImageList.add(productItemImage1);
    List<ProductItem> productItemList = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode(SKU_CODE);
    productItem.setProductItemImages(productItemImageList);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setAttributeCode(ATTR_CODE_1);
    productItemAttribute.setValue(ATTR_VALUE_1);
    ProductItemAttribute descriptiveProductAttribute1 = new ProductItemAttribute();
    descriptiveProductAttribute1.setAttributeCode(ATTR_CODE_2);
    descriptiveProductAttribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    descriptiveProductAttribute1.setMarkForDelete(true);
    productItemAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    productItem.setProductItemAttributes(Arrays.asList(productItemAttribute, descriptiveProductAttribute1));
    productItemList.add(productItem);
    List<ProductAttribute> productAttributeList = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeCode(ATTR_CODE_1);
    productAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    ProductAttribute predifineProductAttribute = new ProductAttribute();
    predifineProductAttribute.setAttributeCode(ATTR_CODE_1);
    predifineProductAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    ProductAttribute descriptiveProductAttribute = new ProductAttribute();
    descriptiveProductAttribute.setAttributeCode(ATTR_CODE_1);
    descriptiveProductAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttributeList.add(productAttribute);
    productAttributeList.add(predifineProductAttribute);
    productAttributeList.add(descriptiveProductAttribute);
    List<ProductImage> productImageList = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImage.setMainImage(true);
    productImage.setOriginalImage(Boolean.FALSE);
    ProductImage productImage1 = new ProductImage();
    productImage1.setMainImage(false);
    productImage1.setOriginalImage(Boolean.TRUE);
    ProductImage productImage2 = new ProductImage();
    productImage2.setMainImage(false);
    productImageList.add(productImage2);
    productImageList.add(productImage);
    productImageList.add(productImage1);
    Product product = new Product.Builder().productCode(PRODUCT_CODE).productItems(productItemList)
        .productAttributes(productAttributeList).productImages(productImageList).postLive(Boolean.TRUE).build();
    return product;
  }

  @BeforeEach
  public void setUp() throws Exception {
    productApprovalDetailStatus = ProductApprovalDetailStatus.PDT_Approve_Product_Kafka_Event_Published_PBP;
  }

  @Test
   void publishUpdatedProductForFinalApprovalTestOk() throws Exception {
    Product product = createProduct();
    product.setMarkForDelete(true);
    product.setReviewType(ReviewType.CONTENT);
    product.setState(WorkflowState.PASSED);
    PDTProductDomainEventModel response =
        approvedProductPublisherService.publishUpdatedProductForFinalApproval(product, true);
    Assertions.assertNotNull(response);
    Assertions.assertTrue(response.isPostLive());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductDomainEventModel.class));
    Assertions.assertTrue(response.isMarkForDelete());
    Assertions.assertEquals(ReviewType.CONTENT.name(), response.getReviewType().name());
    Assertions.assertEquals(WorkflowState.PASSED.name(), response.getState().name());
  }

  @Test
   void publishUpdatedProductForFinalApprovalTestWithOriginalImageFlagOk() throws Exception {
    PDTProductDomainEventModel response =
        approvedProductPublisherService.publishUpdatedProductForFinalApproval(createProductWithOriginalImageFlag(), true);
    Assertions.assertNotNull(response);
    Assertions.assertTrue(response.isPostLive());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductDomainEventModel.class));
  }

  @Test
   void publishUpdatedProductForFinalApprovalFlagWithItemAttributesOk() throws Exception {
    PDTProductDomainEventModel response =
        approvedProductPublisherService.publishUpdatedProductForFinalApproval(createProductWithOriginalImageFlag(), true);
    Assertions.assertNotNull(response);
    Assertions.assertTrue(response.isPostLive());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductDomainEventModel.class));
  }

  @Test
   void publishUpdatedProductForFinalApprovalTestNotOk() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> approvedProductPublisherService.publishUpdatedProductForFinalApproval(null, true));

  }

  @Test
   void publishVendorApprovedEventTest() {
    Mockito.when(kafkaTopicProperties.getProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishVendorApprovedEvent(createProductWithOriginalImageFlag(), false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductVendorApprovedTaskEvent();
  }

  @Test
   void publishVendorApprovedEventPriority1Test() {
    Product product = createProductWithOriginalImageFlag();
    product.setPrioritySeller(PrioritySeller.PRIORITY_1.getPrioritySeller());
    Mockito.when(kafkaTopicProperties.getProductVendorApprovedTaskPriority1Event())
        .thenReturn(DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_1_EVENT_NAME);
    PDTProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishVendorApprovedEvent(product, false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getProductVendorApprovedTaskPriority1Event()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductVendorApprovedTaskPriority1Event();
    Mockito.verify(kafkaTopicProperties).getProductVendorApprovedTaskEvent();
  }

  @Test
   void publishVendorApprovedEventPriority2Test() {
    Product product = createProductWithOriginalImageFlag();
    product.setPrioritySeller(PrioritySeller.PRIORITY_2.getPrioritySeller());
    Mockito.when(kafkaTopicProperties.getProductVendorApprovedTaskPriority2Event())
        .thenReturn(DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_2_EVENT_NAME);
    PDTProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishVendorApprovedEvent(product, false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getProductVendorApprovedTaskPriority2Event()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties).getProductVendorApprovedTaskEvent();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductVendorApprovedTaskPriority2Event();
  }

  @Test
   void publishVendorApprovedEventExceptionTest() {
    PDTProductVendorApprovedEventModel response = null;
    try {
      response =
          approvedProductPublisherService.publishVendorApprovedEvent(null, false);
    } catch (Exception ignored) {
    }finally {
      Assertions.assertNull(response);
    }
  }

  @AfterEach public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaProducer, kafkaTopicProperties);
  }

  @Test
   void convertProductToProductDomainEventModelTest() throws Exception {
    Product product = createProduct();
    product.setBrandApprovalStatus(BRAND_STATUS);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setLength(10.0);
    product.setProductType(1);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setAttributeCode(BRAND_CODE);
    product.getProductItems().get(0).getProductItemAttributes().add(productItemAttribute);
    product.getProductItems().get(0).setDangerousGoodsLevel(0);
    product.getProductItems().get(0).getProductItemAttributes().add(productItemAttribute);
    product.getProductImages().forEach(image -> image.setCommonImage(true));
    product.getProductItems()
        .forEach(item -> item.getProductItemImages().forEach(itemImage -> itemImage.setCommonImage(true)));
    PDTProductDomainEventModel response =
        approvedProductPublisherService.convertProductToProductDomainEventModel(product, false);
    response.setBrandApprovalStatus(BRAND_STATUS);
    response.setBrand(BRAND_NAME);
    response.setBrandCode(BRAND_CODE);
    Assertions.assertFalse(
        response.getPdtImageDomainEventModels().stream().anyMatch(image -> !image.isCommonImage()));
    Assertions.assertFalse(response.getImages().stream().anyMatch(image -> !image.isCommonImage()));
    Assertions.assertFalse(
        response.getProductItems().stream().flatMap(item -> item.getPdtImageDomainEventModels().stream())
            .anyMatch(image -> !image.isCommonImage()));
    Assertions.assertFalse(
        response.getProductItems().stream().flatMap(item -> item.getPdtImageDomainEventModels().stream())
            .anyMatch(image -> !image.isCommonImage()));
  }

  @Test
   void convertProductToProductDomainEventModel_ForDuplicateAttributesTest() throws Exception {
    Product product = createProduct();
    product.setBrandApprovalStatus(BRAND_STATUS);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setLength(10.0);
    product.setProductType(1);
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode(PRODUCT_CODE);
    product.getProductItems().add(productItem);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setAttributeCode(ATTR_CODE_1);
    product.getProductItems().get(0).getProductItemAttributes().add(productItemAttribute);
    product.getProductItems().get(1).getProductItemAttributes().add(productItemAttribute);
    product.getProductImages().forEach(image -> image.setCommonImage(true));
    product.getProductItems()
      .forEach(item -> item.getProductItemImages().forEach(itemImage -> itemImage.setCommonImage(true)));
    PDTProductDomainEventModel response =
      approvedProductPublisherService.convertProductToProductDomainEventModel(product, false);
    response.setBrandApprovalStatus(BRAND_STATUS);
    response.setBrand(BRAND_NAME);
    response.setBrandCode(BRAND_CODE);
    Assertions.assertFalse(
        response.getPdtImageDomainEventModels().stream().anyMatch(image -> !image.isCommonImage()));
    Assertions.assertFalse(response.getImages().stream().anyMatch(image -> !image.isCommonImage()));
    Assertions.assertFalse(
        response.getProductItems().stream().flatMap(item -> item.getPdtImageDomainEventModels().stream())
          .anyMatch(image -> !image.isCommonImage()));

    Assertions.assertFalse(
        response.getProductItems().stream().flatMap(item -> item.getPdtImageDomainEventModels().stream())
          .anyMatch(image -> !image.isCommonImage()));
  }


  @Test
   void convertProductToProductDomainEventModelTestEditedTrue() throws Exception {
    Product product = createProduct();
    product.setEdited(true);
    product.setBrandApprovalStatus(BRAND_STATUS);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    PDTProductDomainEventModel response =
        approvedProductPublisherService.convertProductToProductDomainEventModel(product, false);
    Assertions.assertNotNull(response.getPdtImageDomainEventModels());
  }

  @Test
   void convertProductToProductDomainEventModelVendorNotesTest() throws Exception {
    Product product = createProduct();
    product.setBrandApprovalStatus(BRAND_STATUS);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductNotes(
        "{\"vendorNotes\" : [\"Incomplete or inappropriate content\"],\n" + "\"ContentAdditionalNotes\" : \"notes\"}");
    PDTProductNotesDomainEventModel productNotesResponse = PDTProductNotesDomainEventModel.builder()
        .vendorNotes(List.of("Incomplete or inappropriate content")).contentAdditionalNotes("notes")
        .build();
    product.getProductItems().get(0).setItemNotes(
        "{\"vendorNotes\": [\"Blur or inappropriate images\"],\n" + "\"vendorErrorFields\": [\"images\"]}");
    PDTItemNotesDomainEventModel pdtItemNotesDomainEventModel =
        PDTItemNotesDomainEventModel.builder().vendorNotes(List.of("Blur or inappropriate images")).build();
    Mockito.when(mapper.readValue(anyString(), eq(PDTProductNotesDomainEventModel.class)))
        .thenReturn(productNotesResponse);
    Mockito.when(mapper.readValue(anyString(), eq(PDTItemNotesDomainEventModel.class)))
        .thenReturn(pdtItemNotesDomainEventModel);
    PDTProductDomainEventModel response =
        approvedProductPublisherService.convertProductToProductDomainEventModel(product, false);
    response.setBrandApprovalStatus(BRAND_STATUS);
    response.setBrand(BRAND_NAME);
    response.setBrandCode(BRAND_CODE);
    Assertions.assertEquals(1, response.getProductNotes().getVendorNotes().size());
    Assertions.assertEquals(1,
        response.getProductItems().get(0).getItemNotes().getVendorNotes().size());
  }

  @Test
   void publishUpdatedProductForFinalApprovalExceptionTest() throws Exception {
    Product product = createProduct();
    product.setProductImages(null);
    Assertions.assertThrows(Exception.class,
      () -> approvedProductPublisherService.publishUpdatedProductForFinalApproval(product, true));
  }

  @Test
   void publishEditedVendorApprovedEventTest() {
    Product product = new Product();
    ReflectionTestUtils.setField(approvedProductPublisherService, "replaceEmptyReviewTypeVendorEdited", true);
    product.setProductCode(PRODUCT_CODE);
    product.setReviewType(ReviewType.CONTENT);
    product.setUpdatedBy(UPDATED_BY);
    Mockito.when(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.EDITED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTEditedProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishEditedVendorApprovedEvent(product);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTEditedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getEditedProductVendorApprovedTaskEvent();
  }

  @Test
   void publishEditedVendorApprovedEventNullReviewTypeTest() {
    ReflectionTestUtils.setField(approvedProductPublisherService, "replaceEmptyReviewTypeVendorEdited", true);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setReviewType(null);
    product.setUpdatedBy(UPDATED_BY);
    Mockito.when(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent())
      .thenReturn(DomainEventName.EDITED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTEditedProductVendorApprovedEventModel response =
      approvedProductPublisherService.publishEditedVendorApprovedEvent(product);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
        Mockito.any(PDTEditedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getEditedProductVendorApprovedTaskEvent();
  }

  @Test
  void publishEditedVendorApprovedEventNullReviewTypeSwithOffTest() {
    ReflectionTestUtils.setField(approvedProductPublisherService,
        "replaceEmptyReviewTypeVendorEdited", false);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setReviewType(null);
    product.setUpdatedBy(UPDATED_BY);
    Assertions.assertThrows(Exception.class,
      () -> approvedProductPublisherService.publishEditedVendorApprovedEvent(product));
  }

  @Test
   void publishEditedVendorApprovedEventReviewTypeSwitchOffTest() {
    ReflectionTestUtils.setField(approvedProductPublisherService,
      "replaceEmptyReviewTypeVendorEdited", true);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setUpdatedBy(UPDATED_BY);
    Mockito.when(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent())
      .thenReturn(DomainEventName.EDITED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTEditedProductVendorApprovedEventModel response =
      approvedProductPublisherService.publishEditedVendorApprovedEvent(product);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(kafkaTopicProperties.getEditedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
        Mockito.any(PDTEditedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getEditedProductVendorApprovedTaskEvent();
  }


  @Test
   void publishRevisedVendorApprovedEventTest() {
    Mockito.when(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTRevisedProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(createProductWithOriginalImageFlag(), false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertNull(response.getApprovalType());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTRevisedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getRevisedProductVendorApprovedTaskEvent();
  }

  @Test
   void publishRevisedVendorApprovedEventEditedProductTest() {
    Product product = createProductWithOriginalImageFlag();
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    Mockito.when(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTRevisedProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(ReviewType.CONTENT_AND_IMAGE.toString(), response.getApprovalType());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTRevisedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getRevisedProductVendorApprovedTaskEvent();
  }

  @Test
   void publishRevisedVendorApprovedEventWithEditedStateTest() {
    Product product = createProductWithOriginalImageFlag();
    product.setReviewType(ReviewType.CONTENT);
    Mockito.when(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTRevisedProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTRevisedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getRevisedProductVendorApprovedTaskEvent();
  }

  @Test
   void publishRevisedVendorApprovedEventExceptionTest() {
    Mockito.when(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()).thenReturn(PRODUCT_CODE);
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.eq(PRODUCT_CODE), Mockito.eq(PRODUCT_CODE), Mockito.any());
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> approvedProductPublisherService.publishRevisedVendorApprovedEvent(createProductWithOriginalImageFlag(),
              false));
    } finally {
      Mockito.verify(kafkaProducer)
          .send(Mockito.eq(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
              Mockito.any(PDTRevisedProductVendorApprovedEventModel.class));
      Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getRevisedProductVendorApprovedTaskEvent();
    }
  }

  @Test
   void publishAutoApprovalEvent() {
    PDTAutoApprovalEventModel pdtAutoApprovalEventModel = new PDTAutoApprovalEventModel(PRODUCT_CODE);
    approvedProductPublisherService.publishAutoApprovalEvent(pdtAutoApprovalEventModel);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_EVENT, PRODUCT_CODE, pdtAutoApprovalEventModel);
  }

  @Test
   void publishRevisedVendorApprovedEventEditedRevisedProductTest() {
    Product product = createProductWithOriginalImageFlag();
    product.setReviewType(ReviewType.IMAGE);
    product.setRevised(true);
    product.setEdited(true);
    product.setPostLive(true);
    Mockito.when(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTRevisedProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, true);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertNull(response.getApprovalType());
    Assertions.assertTrue(response.isPostLive());
    Assertions.assertTrue(response.isReviewPending());
    Assertions.assertTrue(Objects.isNull(response.getApprovalType()));
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTRevisedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getRevisedProductVendorApprovedTaskEvent();
  }

  @Test
   void publishRevisedVendorApprovedEventEditedNotRevisedProductTest() {
    Product product = createProductWithOriginalImageFlag();
    product.setReviewType(ReviewType.IMAGE);
    product.setRevised(false);
    product.setEdited(true);
    product.setPostLive(true);
    product.setMarkForDelete(true);
    Mockito.when(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent())
        .thenReturn(DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    PDTRevisedProductVendorApprovedEventModel response =
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, true);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(ReviewType.IMAGE.name(), response.getApprovalType());
    Assertions.assertTrue(response.isPostLive());
    Assertions.assertTrue(response.isReviewPending());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getRevisedProductVendorApprovedTaskEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTRevisedProductVendorApprovedEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getRevisedProductVendorApprovedTaskEvent();
  }

}
