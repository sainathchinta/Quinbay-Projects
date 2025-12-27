package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkImageQueue;
import com.gdn.mta.bulk.entity.ImageQueue;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerClient;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.x.mta.rest.web.request.BulkProcessPostImageRequest;
import com.gdn.x.mta.rest.web.request.BulkProcessProductImageRequest;
import com.gdn.x.mta.rest.web.request.QueueHistoryResult;

@Service("PostImageApiProcessorServiceBean")
public class PostImageApiProcessorServiceBean implements
    GeneralProcessorService<BulkProcessPostImageRequest, Void, Void> {
  
  private static final Logger LOG = LoggerFactory.getLogger(PostImageApiProcessorServiceBean.class);
  
  public static final String IMAGE_QUEUE_LIST_KEY = "ImageQueueKey";  
  public static final String API_IMAGE_QUEUE_MAP_KEY = "ApiImageQueueKey";  
  public static final String VALUE_SEPARATOR = "|";

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;
  
  @Override
  public Void preProcess(BulkProcessPostImageRequest data, Map<String, String> args) {
    return null;
  }

  @Override
  public Void process(BulkProcessPostImageRequest bulkProcessQueue) {
    for(BulkProcessProductImageRequest productImage : bulkProcessQueue.getProductImages()){
      LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "process", 
          bulkProcessQueue.getMerchantCode(), null, bulkProcessQueue.getRequestId(), null, 
          LoggerChannel.KAFKA.getValue(), LoggerClient.MTAAPI.getValue(),
          productImage.getBrand() + ":" + productImage.getProductName(), null);
      
      try{
        Map<String, Object> nameCollection = validImageFileNameBuilder(productImage);
        List<ImageQueue> imageQueues = (List<ImageQueue>) nameCollection.get(IMAGE_QUEUE_LIST_KEY);
        QueueHistoryResult result = (QueueHistoryResult) nameCollection.get(API_IMAGE_QUEUE_MAP_KEY);
        result.setRequestId(bulkProcessQueue.getRequestId());
        result.setMerchantCode(bulkProcessQueue.getMerchantCode());
        result.setSuccess(true);
        result.setValue(result.getValue() + VALUE_SEPARATOR + bulkProcessQueue.getAction());
        BulkImageQueue bulkImageQueue = new BulkImageQueue(imageQueues);

        kafkaProducer.send(kafkaTopicProperties.getBulkProcessImageApiToMtaEvent(), bulkImageQueue);

        kafkaProducer.send(kafkaTopicProperties.getBulkProcessImageResponseEvent(), result);
      } catch(Exception e){
        LOG.error(LoggerStandard.convertErrorTemplate(this, "listen", loggerModel, e, null), e);
      }
    }
    return null;
  }

  private Map<String, Object> validImageFileNameBuilder(BulkProcessProductImageRequest productImage) {
    Map<String, Object> returnMapCollection = new HashMap<String, Object>();
    List<ImageQueue> imageList = new ArrayList<ImageQueue>();
    QueueHistoryResult result = new QueueHistoryResult();
    StringBuilder validImageFilename =
        new StringBuilder(productImage.getBrand().toLowerCase().replaceAll("[^a-zA-Z0-9]+", "-"));
    validImageFilename.append("_");
    validImageFilename.append(productImage.getProductName().toLowerCase()
        .replaceAll("[^a-zA-Z0-9]+", "-"));
    validImageFilename.append("_full");
    int j = 1;
    for (Map.Entry<String, String> entry : productImage.getImages().entrySet()) {
      String k;
      if (j < 9) {
        k = "0" + j;
      }
      else {
        k = String.valueOf(j);
      }
      String[] splitImageFilename = entry.getKey().split("\\.");
      String imageFiletype = splitImageFilename[splitImageFilename.length - 1];
      String validImageFileName = validImageFilename.toString() + k + "." + imageFiletype.toLowerCase();
      imageList.add(new ImageQueue(validImageFileName, entry.getValue()));
      result.setValue(entry.getKey() + VALUE_SEPARATOR + validImageFileName);
      j++;
    }
    returnMapCollection.put(IMAGE_QUEUE_LIST_KEY, imageList);
    returnMapCollection.put(API_IMAGE_QUEUE_MAP_KEY, result);
    return returnMapCollection;
  }
  
}
