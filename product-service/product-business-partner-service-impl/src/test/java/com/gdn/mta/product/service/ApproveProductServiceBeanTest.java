package com.gdn.mta.product.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.service.util.ImageCheckService;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.partners.pbp.service.listener.ImageKafkaSubscriberBean;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

@SuppressWarnings("deprecation")
public class ApproveProductServiceBeanTest {

  private static final String PRODUCT_NAME = "product-name";
  private static final  String PRODUCT_CODE = "MTA-0001";
  private static final  String PRODUCT_IMAGE_LOCATION = "productImageLocationPath";
  private static final  String PRODUCT_ITEM_IMAGE_LOCATION = "productItemImageLocationPath";
  private static final  Long TIMESTAMP = System.currentTimeMillis();
  private static final String SOURCE_DIRECTORY = "/tmp";
  private static final String FINAL_DIRECTORY = "/tmp-final";

  private static final String PRODUCT_IMAGE_HASH_1 = "12345";
  private static final String PRODUCT_IMAGE_HASH_2 = "1234567";
  private static final String ITEM_IMAGE_HASH_1 = "123";
  private static final String ITEM_IMAGE_HASH_2 = "09345";


  @InjectMocks
  private ApproveProductServiceBean approveProductServiceBean;

  @Mock
  private ProductService productService;

  @Mock
  private ProductWfService productWorkflowService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<ScaleEditedImageRequest> scaleEditedImageRequestArgumentCaptor;

  @Mock
  private ImageProcessorService imageProcessorService;

  @Mock
  private ImageCheckService imageCheckService;

  @Mock
  private ImageKafkaSubscriberBean imageKafkaSubscriberBean;

  @Captor
  private ArgumentCaptor<BulkImagesProcessRequest> bulkImagesProcessRequestArgumentCaptor;

  private ProductRequest productRequest;
  private ProductRequest activeImageProductRequest;
  private ProductDetailResponse productDetailResponse;

  private ProductRequest generateProductRequest(boolean active) throws Exception {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setName(PRODUCT_NAME);
    productRequest.setDescription("Description".getBytes());
    productRequest.setLongDescription("Long Description".getBytes());
    productRequest.setSpecificationDetail("Specification Detail");
    productRequest.setUniqueSellingPoint("Unique Selling Point");
    productRequest.setLength(1.0);
    productRequest.setWidth(1.0);
    productRequest.setHeight(1.0);
    productRequest.setWeight(1.0);
    productRequest.setShippingWeight(1.0);
    productRequest.setProductCategories(new ArrayList<ProductCategoryRequest>());
    productRequest.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    productRequest.getProductCategories().add(new ProductCategoryRequest());
    productRequest.getProductAttributes().add(new ProductAttributeRequest());
    productRequest.setProductCode(PRODUCT_CODE);
    productRequest.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    Image productImage = new Image();
    productImage.setActive(active);
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    productRequest.setImages(Arrays.asList(productImage));
    ProductItemRequest productItem = new ProductItemRequest();
    Image productItemImage = new Image();
    productItemImage.setActive(active);
    productItemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    productItem.setImages(Arrays.asList(productItemImage, productItemImage));
    productRequest.setProductItems(Arrays.asList(productItem));
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
    productAttributeRequest.setAttribute(attributeRequest);
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setDescriptiveAttributeValue("-");
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest1.getProductAttributeValues().add(productAttributeValueRequest1);
    productAttributeRequest1.setAttribute(attributeRequest1);
    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeRequest2.setSkuValue(true);
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest2.setAttribute(attributeRequest2);
    AttributeRequest attributeRequest3 = new AttributeRequest();
    attributeRequest3.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    productAttributeValueRequest3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest3.getProductAttributeValues().add(productAttributeValueRequest3);
    productAttributeRequest3.setAttribute(attributeRequest3);
    AttributeRequest attributeRequest4 = new AttributeRequest();
    attributeRequest4.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.DEFINING_ATTRIBUTE);
    ProductAttributeValueRequest productAttributeValueRequest4 = new ProductAttributeValueRequest();
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    productAttributeValueRequest4.setAllowedAttributeValue(allowedAttributeValueRequest);
    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();
    productAttributeRequest4.setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest4.getProductAttributeValues().add(productAttributeValueRequest4);
    productAttributeRequest4.setAttribute(attributeRequest4);
    productRequest.getProductAttributes().add(productAttributeRequest);
    productRequest.getProductAttributes().add(productAttributeRequest1);
    productRequest.getProductAttributes().add(productAttributeRequest2);
    productRequest.getProductAttributes().add(productAttributeRequest3);
    productRequest.getProductAttributes().add(productAttributeRequest4);
    return productRequest;
  }

  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productRequest = new ProductDetailResponse();
    productRequest.setName(PRODUCT_NAME);
    productRequest.setDescription("Description".getBytes());
    productRequest.setLongDescription("Long Description".getBytes());
    productRequest.setSpecificationDetail("Specification Detail");
    productRequest.setUniqueSellingPoint("Unique Selling Point");
    productRequest.setProductCode(PRODUCT_CODE);
    Image productImage = new Image();
    productImage.setLocationPath(PRODUCT_IMAGE_LOCATION);
    Image productImageDeleted = new Image();
    productImageDeleted.setLocationPath(PRODUCT_IMAGE_LOCATION);
    productImageDeleted.setMarkForDelete(true);
    productRequest.setImages(Arrays.asList(productImage, productImageDeleted));
    ProductItemResponse productItem = new ProductItemResponse();
    Image productItemImage = new Image();
    productItemImage.setLocationPath(PRODUCT_ITEM_IMAGE_LOCATION);
    productItem.setImages(Arrays.asList(productItemImage, productItemImage));
    Set set = new HashSet<>();
    set.add(productItem);
    productRequest.setProductItemResponses(set);
    return productRequest;
  }

  private void generateDummyImageFiles(ProductRequest productRequest) {
    productRequest.getImages().forEach(productImage -> {
      try {
        new File(SOURCE_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).createNewFile();
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
    productRequest.getProductItems().forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        try {
          new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).createNewFile();
        } catch (IOException e) {
          e.printStackTrace();
        }
      });
    });
  }

  private void generateDummyImageFiles(ProductDetailResponse productRequest) {
    productRequest.getImages().forEach(productImage -> {
      try {
        new File(SOURCE_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).createNewFile();
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
  }

  private void generateDummyImageActiveFiles(ProductRequest productRequest) {
    productRequest.getImages().forEach(productImage -> {
      try {
        new File(FINAL_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).createNewFile();
      } catch (IOException e) {
        e.printStackTrace();
      }
    });
    productRequest.getProductItems().forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        try {
          new File(FINAL_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).createNewFile();
        } catch (IOException e) {
          e.printStackTrace();
        }
      });
    });
  }

  private void deleteDummyFiles(ProductRequest productRequest) {
    productRequest.getImages().forEach(productImage -> {
      new File(SOURCE_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).delete();
    });
    productRequest.getProductItems().forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        new File(SOURCE_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).delete();
      });
    });
  }

  private void deleteDummyActiveFiles(ProductRequest productRequest) {
    productRequest.getImages().forEach(productImage -> {
      new File(FINAL_DIRECTORY, FilenameUtils.getName(productImage.getLocationPath())).delete();
    });
    productRequest.getProductItems().forEach(productItemRequest -> {
      productItemRequest.getImages().forEach(image -> {
        new File(FINAL_DIRECTORY, FilenameUtils.getName(image.getLocationPath())).delete();
      });
    });
  }

  @BeforeEach
  public void initializeTest() throws Exception{
    MockitoAnnotations.initMocks(this);
    productRequest = generateProductRequest(false);
    activeImageProductRequest = generateProductRequest(true);
    productRequest.setUniqueSellingPoint(null);
    productDetailResponse = generateProductDetailResponse();
    ReflectionTestUtils.setField(approveProductServiceBean, "imageSourceDirectory", SOURCE_DIRECTORY);
    ReflectionTestUtils.setField(approveProductServiceBean, "fullMediumThumbGraphicsSetting",
        Arrays.asList(new CustomGraphicsSettings()));
    Mockito.doNothing().when(productWorkflowService).approveQC(PRODUCT_CODE);
    Mockito.doNothing().when(productWorkflowService).approveImage(PRODUCT_CODE);
    Mockito.doNothing().when(productWorkflowService).approveContent(PRODUCT_CODE);
    Mockito.doNothing().when(productService).updateProductContent(Mockito.any(ProductRequest.class));
    Mockito.doNothing().when(productService).updateProductImage(Mockito.any(ProductRequest.class));

  }

  @Test
  public void approveQC() throws Exception{
    approveProductServiceBean.approveQc(PRODUCT_CODE);
    Mockito.verify(productWorkflowService).approveQC(PRODUCT_CODE);
  }

  @Test
  public void approveContentTest() throws Exception {
    approveProductServiceBean.approveContent(productRequest);
    Mockito.verify(productWorkflowService).approveContent(PRODUCT_CODE);
    Mockito.verify(productService).updateProductContent(Mockito.any(ProductRequest.class));
  }

  @Test
  public void approveImageTest() throws Exception {
    generateDummyImageFiles(productRequest);
    approveProductServiceBean.approveImage(productRequest, 0);
    deleteDummyFiles(productRequest);
    Mockito.verify(productWorkflowService).processImage(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImage(Mockito.any(ProductRequest.class));
  }

  @Test
  public void approveImageDeletedImagesTest() throws Exception {
    generateDummyImageFiles(productRequest);
    productRequest.getImages().forEach(image -> image.setMarkForDelete(true));
    productRequest.getProductItems()
        .forEach(productItemRequest -> productItemRequest.getImages().forEach(image -> image.setMarkForDelete(true)));
    approveProductServiceBean.approveImage(productRequest, 0);
    deleteDummyFiles(productRequest);
    Mockito.verify(productWorkflowService).processImage(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImage(Mockito.any(ProductRequest.class));
  }

  @Test
  public void approveImageFailureTest() throws Exception {
    try {
      Mockito.when(imageCheckService
        .addUniqueImagesAndGetInvalidImage(Mockito.any(ProductRequest.class),
          Mockito.anyMap(), Mockito.anyString())).thenReturn(Pair.of(SOURCE_DIRECTORY,
        Collections.EMPTY_MAP));
      Mockito.doThrow(ApplicationException.class).when(productWorkflowService).processImage(PRODUCT_CODE);
      Assertions.assertThrows(ApplicationException.class, () -> {
        approveProductServiceBean.approveImage(productRequest, 0);
      });
    } finally {
      Mockito.verify(productWorkflowService).processImage(PRODUCT_CODE);
      Mockito.verify(productService).updateProductImage(productRequest);
    }
  }

  @Test
  public void approveContentExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationException()).when(productWorkflowService).approveContent(PRODUCT_CODE);
    try{
      approveProductServiceBean.approveContent(productRequest);
    }catch (ApplicationException e){
      Mockito.verify(productWorkflowService).approveContent(PRODUCT_CODE);
      Mockito.verify(productService).updateProductContent(Mockito.any(ProductRequest.class));
    }
  }

  @Test
  public void approveImageExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(productWorkflowService).processImage(PRODUCT_CODE);
    try{
      approveProductServiceBean.approveImage(productRequest, 0);
    }catch (Exception e){
      Mockito.verify(productWorkflowService).processImage(PRODUCT_CODE);
      Mockito.verify(productService).updateProductImage(Mockito.any(ProductRequest.class));
    }
  }

  @Test
  public void processImageTest() throws Exception {
    generateDummyImageFiles(productRequest);
    approveProductServiceBean.processImage(productDetailResponse);
    deleteDummyFiles(productRequest);
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImage(Mockito.any(ProductDetailResponse.class), Mockito.anyMap(),
        Mockito.anyString());
    Mockito.verify(imageProcessorService).scaleImage(Mockito.any(BulkImagesProcessRequest.class));
  }

  @Test
  public void processImageFailureTest() throws Exception {
    try {
      Mockito.when(imageCheckService
        .addUniqueImagesAndGetInvalidImage(Mockito.any(ProductDetailResponse.class), Mockito.anyMap(),
          Mockito.anyString())).thenReturn(SOURCE_DIRECTORY);
      ReflectionTestUtils.setField(approveProductServiceBean, "imageSourceDirectory", SOURCE_DIRECTORY + 1);
      approveProductServiceBean.processImage(productDetailResponse);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(imageCheckService)
        .addUniqueImagesAndGetInvalidImage(Mockito.any(ProductDetailResponse.class), Mockito.anyMap(),
          Mockito.anyString());
      Mockito.verify(productWorkflowService, Mockito.times(0)).rejectImage(productRequest.getProductCode());
    }
  }

  @Test
  public void processEditedImageTest() throws Exception {
    generateDummyImageActiveFiles(activeImageProductRequest);
    approveProductServiceBean.processEditedImage(activeImageProductRequest);
    deleteDummyActiveFiles(activeImageProductRequest);
    Mockito.verify(imageProcessorService).scaleEditedImages(scaleEditedImageRequestArgumentCaptor.capture());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImageForEditedProducts(Mockito.any(), Mockito.anyMap(),
        Mockito.anyString());
    Assertions.assertEquals(0, scaleEditedImageRequestArgumentCaptor.getValue().getImageRequests().stream()
        .filter(scaleImageRequest -> !scaleImageRequest.isActive()).count());
  }

  @Test
  public void processEditedImageMFDTrueTest() throws Exception {
    activeImageProductRequest.getImages().get(0).setMarkForDelete(true);
    generateDummyImageActiveFiles(activeImageProductRequest);
    approveProductServiceBean.processEditedImage(activeImageProductRequest);
    deleteDummyActiveFiles(activeImageProductRequest);
    Mockito.verify(imageProcessorService).scaleEditedImages(scaleEditedImageRequestArgumentCaptor.capture());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImageForEditedProducts(Mockito.any(), Mockito.anyMap(),
        Mockito.anyString());
    Assertions.assertEquals(0, scaleEditedImageRequestArgumentCaptor.getValue().getImageRequests().stream()
        .filter(scaleImageRequest -> !scaleImageRequest.isActive()).count());
  }

  @Test
  public void processEditedImageTestWithActiveFalseImages() throws Exception {
    generateDummyImageFiles(productRequest);
    approveProductServiceBean.processEditedImage(productRequest);
    deleteDummyFiles(productRequest);
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImageForEditedProducts(Mockito.any(), Mockito.anyMap(),
        Mockito.anyString());
    Mockito.verify(imageProcessorService).scaleEditedImages(scaleEditedImageRequestArgumentCaptor.capture());
  }

  @Test
  public void processEditedImageExceptionTest() throws Exception {
    Mockito.when(imageCheckService
      .addUniqueImagesAndGetInvalidImageForEditedProducts(Mockito.any(), Mockito.anyMap(),
        Mockito.anyString())).thenReturn(SOURCE_DIRECTORY);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        approveProductServiceBean.processEditedImage(productRequest);
      });
    }
    finally {
      Mockito.verify(imageCheckService)
        .addUniqueImagesAndGetInvalidImageForEditedProducts(Mockito.any(), Mockito.anyMap(), Mockito.anyString());
    }
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productWorkflowService);
    Mockito.verifyNoMoreInteractions(productStatusPublisherService);
    Mockito.verifyNoMoreInteractions(imageProcessorService);
    Mockito.verifyNoMoreInteractions(imageCheckService);
  }

  @Test
  public void processImageForRevisedProductTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    productDetailResponse.getImages().forEach(image -> image.setCommonImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    approveProductServiceBean.processImageForRevisedProduct(productDetailResponse);
    deleteDummyFiles(productRequest);
    Mockito.verify(imageProcessorService).scaleImage(bulkImagesProcessRequestArgumentCaptor.capture());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImageForRevisedProduct(Mockito.any(), Mockito.anyMap(),
        Mockito.anyString());
    Assertions.assertFalse(bulkImagesProcessRequestArgumentCaptor.getValue().getImageRequests().stream()
        .anyMatch(imageRequest -> !imageRequest.isCommonImage()));
  }

  @Test
  public void processImageForRevisedProductDuplicateImageTest() throws Exception {
    generateDummyImageFiles(productDetailResponse);
    productDetailResponse.setImages(
        Arrays.asList(productDetailResponse.getImages().get(0), productDetailResponse.getImages().get(0)));
    productDetailResponse.getImages().forEach(image -> image.setCommonImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    approveProductServiceBean.processImageForRevisedProduct(productDetailResponse);
    deleteDummyFiles(productRequest);
    Mockito.verify(imageProcessorService).scaleImage(bulkImagesProcessRequestArgumentCaptor.capture());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImageForRevisedProduct(Mockito.any(), Mockito.anyMap(),
        Mockito.anyString());
    Assertions.assertFalse(bulkImagesProcessRequestArgumentCaptor.getValue().getImageRequests().stream()
        .anyMatch(imageRequest -> !imageRequest.isCommonImage()));
  }


  @Test
  public void processImageForRevisedProductFailureTest() throws Exception {
    try {
      Mockito.when(imageCheckService
        .addUniqueImagesAndGetInvalidImageForRevisedProduct(Mockito.any(ProductDetailResponse.class),
          Mockito.anyMap(), Mockito.anyString())).thenReturn(SOURCE_DIRECTORY);
      ReflectionTestUtils.setField(approveProductServiceBean, "imageSourceDirectory", SOURCE_DIRECTORY + 1);
      approveProductServiceBean.processImageForRevisedProduct(productDetailResponse);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(imageCheckService)
        .addUniqueImagesAndGetInvalidImageForRevisedProduct(Mockito.any(), Mockito.anyMap(),
          Mockito.anyString());
      Mockito.verify(productWorkflowService, Mockito.times(1)).rejectImage(productRequest.getProductCode());
    }
  }

  @Test
  public void testProcessImageWithUnavailableImage() throws Exception {
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    Map<String, Pair<ImageRequest, String>> imageRequestMap = new HashMap<>();
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    productRequest.getImages().get(0).setMarkForDelete(false);
    productRequest.getProductItems().get(0).getImages().get(0).setHashCode(ITEM_IMAGE_HASH_1);
    productRequest.getProductItems().get(0).getImages().get(0).setMarkForDelete(false);
    productImageHashCodeMapDB.put(PRODUCT_IMAGE_HASH_1, productRequest.getImages().get(0));
    Map<String, CommonImagePathDto> commonImagePathDtoMap = new HashMap<>();
    itemImageHashCodeMapDB.put(ITEM_IMAGE_HASH_1,
      productRequest.getProductItems().get(0).getImages().get(0));
    Mockito.when(imageCheckService
      .addUniqueImagesAndGetInvalidImage(Mockito.any(ProductRequest.class),
        Mockito.anyMap(), Mockito.anyString())).thenReturn(Pair.of(StringUtils.EMPTY,
      Collections.emptyMap()));
    approveProductServiceBean.processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
    Mockito.verify(imageKafkaSubscriberBean, Mockito.never())
      .kafkaEventHelper(Mockito.any(), Mockito.any());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImage(productRequest, uniqueImages,
        SOURCE_DIRECTORY);
  }

  @Test
  public void testProcessImageWithImageEligibleForScaling() throws Exception {
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    Map<String, Pair<ImageRequest, String>> imageRequestMap = new HashMap<>();
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductRequest productRequest = new ProductRequest();
    Map<String, CommonImagePathDto> commonImagePathDtoMap = new HashMap<>();
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
    productRequest.setImages(Collections.singletonList(productImage));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(Collections.singletonList(itemImage));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    productImageHashCodeMapDB.put(PRODUCT_IMAGE_HASH_1, productRequest.getImages().get(0));
    itemImageHashCodeMapDB.put(ITEM_IMAGE_HASH_1,
      productRequest.getProductItems().get(0).getImages().get(0));
    commonImagePathDtoMap.put(PRODUCT_IMAGE_HASH_1,new CommonImagePathDto(PRODUCT_IMAGE_LOCATION,
      false));
    commonImagePathDtoMap.put(ITEM_IMAGE_HASH_1,new CommonImagePathDto(PRODUCT_ITEM_IMAGE_LOCATION,
      false));
    Mockito.when(imageCheckService
      .addUniqueImagesAndGetInvalidImage(productRequest,
        uniqueImages, SOURCE_DIRECTORY)).thenReturn(Pair.of(StringUtils.EMPTY,Collections.emptyMap()));
    approveProductServiceBean.processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
    Mockito.verify(imageKafkaSubscriberBean, Mockito.never())
      .kafkaEventHelper(Mockito.any(), Mockito.any());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImage(productRequest, uniqueImages,
        SOURCE_DIRECTORY);
  }

  @Test
  public void testProcessImageWithImageEligibleForScalingFalse() throws Exception {
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    Map<String, Pair<ImageRequest, String>> imageRequestMap = new HashMap<>();
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductRequest productRequest = new ProductRequest();
    Map<String, CommonImagePathDto> commonImagePathDtoMap = new HashMap<>();
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
    productRequest.setImages(Collections.singletonList(productImage));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(Collections.singletonList(itemImage));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    productImageHashCodeMapDB.put(PRODUCT_IMAGE_HASH_1, productRequest.getImages().get(0));
    itemImageHashCodeMapDB.put(ITEM_IMAGE_HASH_1,
      productRequest.getProductItems().get(0).getImages().get(0));
    commonImagePathDtoMap.put(PRODUCT_IMAGE_HASH_1,new CommonImagePathDto(PRODUCT_IMAGE_LOCATION,
      false));
    commonImagePathDtoMap.put(ITEM_IMAGE_HASH_1,new CommonImagePathDto(PRODUCT_ITEM_IMAGE_LOCATION,
      false));
    Mockito.when(imageCheckService
      .addUniqueImagesAndGetInvalidImage(productRequest,
        uniqueImages, SOURCE_DIRECTORY)).thenReturn(Pair.of(StringUtils.EMPTY,commonImagePathDtoMap));
    approveProductServiceBean.processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
    Mockito.verify(imageKafkaSubscriberBean)
      .kafkaEventHelper(Mockito.any(), Mockito.any());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImage(productRequest, uniqueImages,
        SOURCE_DIRECTORY);
  }

  @Test
  public void testProcessImageWithImageEligibleForScalingTrue() throws Exception {
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    Map<String, Pair<ImageRequest, String>> imageRequestMap = new HashMap<>();
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    ProductRequest productRequest = new ProductRequest();
    Map<String, CommonImagePathDto> commonImagePathDtoMap = new HashMap<>();
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
    productRequest.setImages(Collections.singletonList(productImage));
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setImages(Collections.singletonList(itemImage));
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    productImageHashCodeMapDB.put(PRODUCT_IMAGE_HASH_2, productRequest.getImages().get(0));
    itemImageHashCodeMapDB.put(ITEM_IMAGE_HASH_2,
      productRequest.getProductItems().get(0).getImages().get(0));
    commonImagePathDtoMap.put(PRODUCT_IMAGE_HASH_1,new CommonImagePathDto(PRODUCT_IMAGE_LOCATION,
      false));
    commonImagePathDtoMap.put(ITEM_IMAGE_HASH_1,new CommonImagePathDto(PRODUCT_ITEM_IMAGE_LOCATION,
      false));
    Mockito.when(imageCheckService
      .addUniqueImagesAndGetInvalidImage(productRequest,
        uniqueImages, SOURCE_DIRECTORY)).thenReturn(Pair.of(StringUtils.EMPTY,commonImagePathDtoMap));
    approveProductServiceBean.processImageWithScalingEligibility(productRequest,0,
      productImageHashCodeMapDB,itemImageHashCodeMapDB);
    Mockito.verify(imageKafkaSubscriberBean,Mockito.never())
      .kafkaEventHelper(Mockito.any(), Mockito.any());
    Mockito.verify(imageCheckService)
      .addUniqueImagesAndGetInvalidImage(productRequest, uniqueImages,
        SOURCE_DIRECTORY);
    Mockito.verify(imageProcessorService).scaleImage(bulkImagesProcessRequestArgumentCaptor.capture());

  }

  @Test
  public void testProcessImageWithUnavailableWithInvalidImage() throws Exception {
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    Map<String, Pair<ImageRequest, String>> imageRequestMap = new HashMap<>();
    Map<String, Image> productImageHashCodeMapDB = new HashMap<>();
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    productRequest.getImages().get(0).setMarkForDelete(false);
    productRequest.getProductItems().get(0).getImages().get(0).setHashCode(ITEM_IMAGE_HASH_1);
    productRequest.getProductItems().get(0).getImages().get(0).setMarkForDelete(false);
    productImageHashCodeMapDB.put(PRODUCT_IMAGE_HASH_1, productRequest.getImages().get(0));
    Map<String, CommonImagePathDto> commonImagePathDtoMap = new HashMap<>();
    itemImageHashCodeMapDB.put(ITEM_IMAGE_HASH_1,
      productRequest.getProductItems().get(0).getImages().get(0));
    Mockito.when(imageCheckService
      .addUniqueImagesAndGetInvalidImage(Mockito.any(ProductRequest.class),
        Mockito.anyMap(), Mockito.anyString())).thenReturn(Pair.of(PRODUCT_IMAGE_LOCATION,
      Collections.emptyMap()));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        approveProductServiceBean.processImageWithScalingEligibility(productRequest, 0,
            productImageHashCodeMapDB, itemImageHashCodeMapDB);
      });
    }
    finally {
      Mockito.verify(productWorkflowService).rejectImage(PRODUCT_CODE);
      Mockito.verify(imageCheckService)
        .addUniqueImagesAndGetInvalidImage(productRequest, uniqueImages,
          SOURCE_DIRECTORY);
    }}

  @Test
  public void updateImageWithpoutHashCodeTest() throws Exception {
    approveProductServiceBean.updateImageAndAvoidHashCodeRegeneration(productRequest);
    Mockito.verify(productService).updateProductImage(productRequest);
  }

}
