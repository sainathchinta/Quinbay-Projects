package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkImageQueueV2;
import com.gdn.mta.bulk.models.BulkProcessPostImageV2Request;
import com.gdn.mta.bulk.models.BulkProcessPostItemImageV2Request;
import com.gdn.mta.bulk.models.BulkProcessProductImageV2Request;
import com.gdn.mta.bulk.repository.ProductLevel1Repository;
import com.gdn.x.mta.rest.web.request.QueueHistoryResult;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductItemImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.ImagePathResponse;

public class PostImageApiProcessorV2ServiceBeanTest {
  
  @InjectMocks
  private PostImageApiProcessorV2ServiceBean postImageService;
  @Mock
  private ProductLevel1Repository productLv1Repository;
  @Mock
  private KafkaPublisher kafkaProducer;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  
  private static String MERCHANT_CODE = "TOQ-15130";
  private static String PRODUCT_CODE = "MTA-001";
  private BulkProcessPostImageV2Request bulkRequest;
  private GeneratedProductImagesPathResponse generatedPathResp;
  
  @Test
  public void processTest() throws Exception {
    bulkRequest.setRequestId("id");
    bulkRequest.setUsername("username");
    bulkRequest.setMerchantCode("merchantCode");
    Mockito.when(productLv1Repository.replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
      .thenReturn(generatedPathResp);
    postImageService.process(bulkRequest);
    Mockito.verify(productLv1Repository).replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageApiToMtaEventV2()), Mockito.any(BulkImageQueueV2.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(QueueHistoryResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageApiToMtaEventV2();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }

  @Test
  public void processWithNullItemRequsetTest() throws Exception {
    bulkRequest.setRequestId("id");
    bulkRequest.setUsername("username");
    bulkRequest.setMerchantCode("merchantCode");
    Mockito.when(
        productLv1Repository.replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(generatedPathResp);
    bulkRequest.setProductItem(null);
    postImageService.process(bulkRequest);
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.eq(kafkaTopicProperties.getBulkProcessImageApiToMtaEventV2()), Mockito.any());
    Mockito.verify(kafkaProducer, Mockito.times(2)).send(Mockito.eq(kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any());
    Mockito.verify(productLv1Repository)
        .replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageApiToMtaEventV2();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithNullItemImageRequsetTest() throws Exception {
    Mockito.when(productLv1Repository.replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
      .thenReturn(generatedPathResp);

    bulkRequest.getProductItem().get(0).setImages(null);
    Assertions.assertThrows(RuntimeException.class, () -> postImageService.process(bulkRequest));
  }

  @Test
  public void processWithNullImageRequsetTest() throws Exception {
    Mockito.when(productLv1Repository.replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
      .thenReturn(generatedPathResp);

    bulkRequest.setImages(null);
    Assertions.assertThrows(RuntimeException.class, () -> postImageService.process(bulkRequest));
  }
  
  @Test
  public void preProcess() throws Exception {
    postImageService.preProcess(null, null);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaProducer, kafkaTopicProperties);
  }
  
  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    
    generatedPathResp = new GeneratedProductImagesPathResponse();
    generatedPathResp.setProductCode(PRODUCT_CODE);
    generatedPathResp.setImages(new ArrayList<ImagePathResponse>());
    generatedPathResp.getImages().add(new ImagePathResponse("/MTA-0001/product_image-1.jpg", true));
    generatedPathResp.getImages().add(new ImagePathResponse("/MTA-0001/product_image-2.jpg", false));
    generatedPathResp.getImages().add(new ImagePathResponse("/MTA-0001/product_image-3.jpg", false));
    
    generatedPathResp.setProductItems(new ArrayList<GeneratedProductItemImagesPathResponse>());
    List<ImagePathResponse> generatedImagePath = new ArrayList<>();
    generatedImagePath.add(new ImagePathResponse("/MTA-001/product-image-4.jpg", true));
    generatedImagePath.add(new ImagePathResponse("/MTA-001/product-image-5.jpg", false));
    generatedImagePath.add(new ImagePathResponse("/MTA-001/product-image-6.jpg", false));
    generatedImagePath.add(new ImagePathResponse("/MTA-001/product-image-7.jpg", false));
    generatedPathResp.getProductItems().add(new GeneratedProductItemImagesPathResponse("MTA-001-001", generatedImagePath));
    
    generatedImagePath = new ArrayList<>();
    generatedImagePath.add(new ImagePathResponse("/MTA-001/product-image-8.jpg", true));
    generatedImagePath.add(new ImagePathResponse("/MTA-001/product-image-9.jpg", false));
    generatedPathResp.getProductItems().add(new GeneratedProductItemImagesPathResponse("MTA-001-002", generatedImagePath));
    
    bulkRequest = new BulkProcessPostImageV2Request();
    bulkRequest.setMerchantCode(MERCHANT_CODE);
    bulkRequest.setProductCode(PRODUCT_CODE);
    bulkRequest.setImages(new ArrayList<BulkProcessProductImageV2Request>());
    bulkRequest.getImages().add(new BulkProcessProductImageV2Request(false, "imagefile-product-1"));
    bulkRequest.getImages().add(new BulkProcessProductImageV2Request(true, "imagefile-product-2-main"));
    bulkRequest.getImages().add(new BulkProcessProductImageV2Request(false, "imagefile-product-3"));
    
    List<BulkProcessProductImageV2Request> itemImageRequest = new ArrayList<>();
    itemImageRequest.add(new BulkProcessProductImageV2Request(false, "imagefile-item-1"));
    itemImageRequest.add(new BulkProcessProductImageV2Request(false, "imagefile-item-2"));
    itemImageRequest.add(new BulkProcessProductImageV2Request(true, "imagefile-item-3-main"));
    itemImageRequest.add(new BulkProcessProductImageV2Request(false, "imagefile-item-4"));
    bulkRequest.setProductItem(new ArrayList<BulkProcessPostItemImageV2Request>());
    bulkRequest.getProductItem().add(new BulkProcessPostItemImageV2Request("MTA-001-001", itemImageRequest));
    
    itemImageRequest = new ArrayList<>();
    itemImageRequest.add(new BulkProcessProductImageV2Request(true, "imagefile-item-5-main"));
    itemImageRequest.add(new BulkProcessProductImageV2Request(false, "imagefile-item-6"));
    bulkRequest.getProductItem().add(new BulkProcessPostItemImageV2Request("MTA-001-002", itemImageRequest));
    
    itemImageRequest = new ArrayList<>();
    itemImageRequest.add(new BulkProcessProductImageV2Request(true, "imagefile-item-5-main"));
    itemImageRequest.add(new BulkProcessProductImageV2Request(false, "imagefile-item-6"));
    bulkRequest.getProductItem().add(new BulkProcessPostItemImageV2Request("unknow-code", itemImageRequest));
  }
  
}
