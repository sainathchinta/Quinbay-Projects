package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.entity.BulkImageQueueV2;
import com.gdn.mta.bulk.entity.ImageTmpMap;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerClient;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.models.BulkProcessPostImageV2Request;
import com.gdn.mta.bulk.models.BulkProcessPostItemImageV2Request;
import com.gdn.mta.bulk.models.BulkProcessProductImageV2Request;
import com.gdn.mta.bulk.repository.ProductLevel1Repository;
import com.gdn.x.mta.rest.web.request.QueueHistoryResult;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductItemImagesRequest;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductItemImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.ImagePathResponse;

/**
 * @author agie.falah
 *
 */
@Service("PostImageApiProcessorV2ServiceBean")
public class PostImageApiProcessorV2ServiceBean implements
    GeneralProcessorService<BulkProcessPostImageV2Request, Void, Void> {
  
  private static final Logger LOG = LoggerFactory.getLogger(PostImageApiProcessorV2ServiceBean.class);
  
  public static final String IMAGE_QUEUE_LIST_KEY = "ImageQueueKey";  
  public static final String API_IMAGE_QUEUE_MAP_KEY = "ApiImageQueueKey";  
  public static final String VALUE_SEPARATOR = "|";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private ProductLevel1Repository productLv1Repository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;
  
  @Override
  public Void preProcess(BulkProcessPostImageV2Request data, Map<String, String> args) {
    return null;
  }
  
  /**
   * Build request that will be processed by PCB to generate product's image path name on PCB level
   * 
   * @param bulkQueue
   * @return
   * @throws Exception
   */
  private ReplaceProductImagesRequest buildProductImageRequest(BulkProcessPostImageV2Request bulkQueue) throws
      ApplicationRuntimeException {
    if(CollectionUtils.isEmpty(bulkQueue.getImages())){
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, "Product image for:" + bulkQueue.getProductCode() + " is empty");
    }
    ReplaceProductImagesRequest imagesRequest = new ReplaceProductImagesRequest();
    imagesRequest.setProductCode(bulkQueue.getProductCode());
    imagesRequest.setGeneratedImageCount(bulkQueue.getImages().size());
    imagesRequest.setProductItem(new ArrayList<ReplaceProductItemImagesRequest>());
    
    if(CollectionUtils.isNotEmpty(bulkQueue.getProductItem())) {
      for(BulkProcessPostItemImageV2Request item : bulkQueue.getProductItem()){
        if(CollectionUtils.isEmpty(item.getImages())){
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, "Product item image for:" + item.getProductItemCode() + " is empty");
        }
        ReplaceProductItemImagesRequest itemRequest = 
            new ReplaceProductItemImagesRequest(item.getProductItemCode(), item.getImages().size());
        imagesRequest.getProductItem().add(itemRequest);
      }
    }
    return imagesRequest;
  }
  
  /**
   * Sort and build DTO for specified product item's image
   * 
   * @param source
   * @param productItemCode
   * @return
   */
  private List<ImagePathResponse> getGeneratedItemImagePath(List<GeneratedProductItemImagesPathResponse> source, String productItemCode) {
    for(GeneratedProductItemImagesPathResponse generatedItemImagePath : source){
      if(generatedItemImagePath.getSkuCode().equals(productItemCode)){
        return generatedItemImagePath.getImages();
      }
    }
    return new ArrayList<ImagePathResponse>();
  }
  
  /**
   * Sort image request list to put main image on first element of list
   * 
   * @param source
   * @return
   */
  private List<BulkProcessProductImageV2Request> sortImageFile(List<BulkProcessProductImageV2Request> source){
    int mainImageIndex = 0;
    for(BulkProcessProductImageV2Request data : source){
      if(data.isMainImage()){
        break;
      }
      mainImageIndex++;
    }
    
    if(mainImageIndex != 0){
      BulkProcessProductImageV2Request firstElement = source.get(0);
      source.set(0, source.get(mainImageIndex));
      source.set(mainImageIndex, firstElement);
    }
    return source;
  }
  
  /**
   * Append current resultList with sorted list given
   * 
   * @param tmpDir
   * @param resultList
   * @param unsortedImageList
   * @param generatedPath
   */
  private void appendAndSortImageFile(String tmpDir, List<ImageTmpMap> resultList, List<BulkProcessProductImageV2Request> unsortedImageList, 
      List<ImagePathResponse> generatedPath){
    if(CollectionUtils.isNotEmpty(generatedPath)){
      int i = 0;
      for(BulkProcessProductImageV2Request productImage : this.sortImageFile(unsortedImageList)){
        resultList.add(new ImageTmpMap(generatedPath.get(i).getImagePath(), "/" + tmpDir + "/" + productImage.getImagePath()));
        i++;
      }
    }
  }
  
  /**
   * Build request that will be sent to MTA to map generated product's image from PCB
   * 
   * @param generatedData
   * @param requestData
   * @return
   */
  private List<ImageTmpMap> buildProductImageData(GeneratedProductImagesPathResponse generatedData, 
      BulkProcessPostImageV2Request requestData){
    List<ImageTmpMap> imageTmpMapList = new ArrayList<>();
    this.appendAndSortImageFile(requestData.getTmpDir(), imageTmpMapList, requestData.getImages(), generatedData.getImages());
    if(CollectionUtils.isNotEmpty(requestData.getProductItem())){
      for(BulkProcessPostItemImageV2Request productItemReq : requestData.getProductItem()){
        this.appendAndSortImageFile(requestData.getTmpDir(), imageTmpMapList, productItemReq.getImages(), 
            this.getGeneratedItemImagePath(generatedData.getProductItems(), productItemReq.getProductItemCode()));
      }
    }
    return imageTmpMapList;
  }
  
  
  @Override
  public Void process(BulkProcessPostImageV2Request bulkProcessQueue) throws Exception {
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "process", 
        bulkProcessQueue.getMerchantCode(), null, bulkProcessQueue.getRequestId(), null, 
        LoggerChannel.KAFKA.getValue(), LoggerClient.MTAAPI.getValue(),
        null, null);
    
    try{
      GeneratedProductImagesPathResponse generatedProductImage = productLv1Repository.replaceProductImages(
          bulkProcessQueue.getRequestId(), bulkProcessQueue.getUsername(), bulkProcessQueue.getMerchantCode(), 
            this.buildProductImageRequest(bulkProcessQueue));
      List<ImageTmpMap> imageQueue = this.buildProductImageData(generatedProductImage, bulkProcessQueue);
      
      QueueHistoryResult result = new QueueHistoryResult();
      result.setRequestId(bulkProcessQueue.getRequestId());
      result.setMerchantCode(bulkProcessQueue.getMerchantCode());
      result.setSuccess(true);
      result.setValue(bulkProcessQueue.getAction() + ": uploaded " + imageQueue.size() + " image(s)");

      BulkImageQueueV2 bulkImageQueue = new BulkImageQueueV2(bulkProcessQueue.getTmpDir(), imageQueue);

      kafkaProducer.send(kafkaTopicProperties.getBulkProcessImageApiToMtaEventV2(), bulkImageQueue);
      kafkaProducer.send(kafkaTopicProperties.getBulkProcessImageResponseEvent(), result);
    } catch(Exception e){
      LOG.error(LoggerStandard.convertErrorTemplate(this, "listen", loggerModel, e, null), e);
      throw e;
    }
    return null;
  }
  
}
