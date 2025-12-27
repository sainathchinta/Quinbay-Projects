package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.VatUpdateDto;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.x.businesspartner.dto.ProfileResponse;


/**
 * Created by virajjasani on 26/07/16.
 */
public interface BulkUpdateService {

  /**
   * Pre processing of Bulk Update Request before sending data to Queue
   *
   * @param storeId
   * @param bulkUpdateProcessDTO
   * @param accessiblePickupPoints
   * @throws Exception
   */
  void preProcessBulkUpdate(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO,
      Set<String> accessiblePickupPoints)
      throws Exception;

  /**
   * Pre processing of Bulk Update Request for Campaign Product before sending data to Queue
   *
   * @param storeId
   * @param bulkAddCampaignProductDTO
   * @throws Exception
   */
  void preProcessCampaignProductBulkUpdate(String storeId, String requestId,
      BulkAddCampaignProductDTO bulkAddCampaignProductDTO) throws Exception;



  /**
   * Post Processing of Bulk Update Request after retrieving data from Queue
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */
  void processBulkUpdateEvent(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Post Processing of Bulk Update Request after retrieving data from Queue
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */
  void processBulkUpdate(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Process L5 updates
   *
   * @param bulkUpdateEventModel
   * @param privilegedMap
   * @param listBulkUpdateErrorDTO
   * @param validationPassedData
   * @param counter
   * @param isMultiPickupPointSeller
   * @param createdBy
   * @param profileResponse
   * @return
   * @throws Exception
   */
  List<BulkUpdateSuccessDTO> getBulkUpdateSuccessDTOList(BulkUpdateEventModel bulkUpdateEventModel,
      Map<String, Boolean> privilegedMap, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
      List<Map<String, String>> validationPassedData, BulkUpdateErrorCounter counter, boolean isMultiPickupPointSeller,
      String createdBy, ProfileResponse profileResponse) throws Exception;

  /**
   * Get pickup point data
   *
   * @param businessPartnerCode
   * @param pickupPointCodes
   * @return
   * @throws Exception
   */
  List<String> getPickUpPoints(String businessPartnerCode, Set<String> pickupPointCodes) throws Exception;

  /**
   * Get minimum price
   *
   * @param storeId
   * @return
   */
  Integer getMinimumPrice(String storeId);

  /**
   * Post Processing of Bulk Update Request for Campaign Product after retrieving data from Queue
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */
  void processCampaignProductBulkUpdate(BulkAddCampaignProductQueue bulkUpdateQueue)
      throws Exception;

  /**
   * @param bulkProcess
   * @throws Exception
   */
  void saveBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> dataMaps) throws Exception;

  /**
   * @param bulkProcess
   * @throws Exception
   */
  void saveBulkProcessDataForVatUpdate(BulkProcess bulkProcess, List<VatUpdateDto> vatUpdateDtoList) throws Exception;

  /**
   *
   * @param storeId
   * @param requestId
   * @param bulkUpdateProcessDTO
   */
  void preProcessBulkArchiveItems(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO);

  /**
   * Pre Process bulk archive productSkus
   *
   * @param storeId
   * @param requestId
   * @param bulkUpdateProcessDTO
   */
  void preProcessBulkArchiveProducts(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO);

  /**
   * Pre process bulk update off2On
   *
   * @param storeId
   * @param requestId
   * @param bulkUpdateProcessDTO
   */
  void preProcessBulkUpdateOff2On(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO);

  /**
   * Process bulk Archive of products
   * @param bulkUpdateQueue
   */
  void processBulkArchiveProducts(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Process bulk vat update of products
   * @param bulkUpdateQueue
   */
  void processBulkVatUpdate(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Bulk update off2on
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */
  void processBulkUpdateOff2On(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Process L5 update
   *
   * @param businessPartnerCode
   * @param privilegedMap
   * @param listBulkUpdateErrorDTO
   * @param validationPassedData
   * @param counter
   * @param isMultiPickupPointSeller
   * @param createdBy
   * @param itemPickupPointListingL3Responses
   * @param clientHost
   * @param profileResponse
   * @return
   * @throws Exception
   */
  List<BulkUpdateSuccessDTO> processBulkUpdateL5(String businessPartnerCode, Map<String, Boolean> privilegedMap,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<Map<String, String>> validationPassedData,
      BulkUpdateErrorCounter counter, boolean isMultiPickupPointSeller, String createdBy,
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3Responses, String clientHost,
      ProfileResponse profileResponse)
      throws Exception;

  /**
   * Get itemPickupPoint responses
   *
   * @param itemPickupPointListingL3Responses
   * @param itemPickupPointListingL3Request
   * @return
   * @throws Exception
   */
  List<ItemPickupPointListingL3Response> getItemPickupPointListingL3Responses(
      List<ItemPickupPointListingL3Response> itemPickupPointListingL3Responses,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception;

  /**
   * Get L5 response from PBP
   *
   * @param size
   * @param itemPickupPointListingL3Request
   * @return
   * @throws Exception
   */
  List<ItemPickupPointListingL3Response> getItemPickupPointListingL3ResponseList(int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception;

  /**
   * Api to process bulk generic event
   *
   * @param bulkUpdateEventModel
   * @return
   * @throws Exception
   */
  void processEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;



  /**
   * Post Processing of Bulk Update Request after retrieving data from Queue
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */

  void processBulkUpdateV2(BulkUpdateQueue bulkUpdateQueue);

  /**
   * Api to save the data for update
   *
   * @param bulkProcess
   * @param productDataFromExcel
   * @param privilegedMap
   * @return
   */
  int generateBulkProcessData(BulkProcess bulkProcess, List<Map<String, String>> productDataFromExcel,
      Map<String, Boolean> privilegedMap, ProfileResponse profileResponse) throws Exception;

  /**
   * Post Processing of Bulk Update Request after retrieving data from Queue
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */
  void processBulkUpdateItem(BulkUpdateEventModel bulkUpdateQueue) throws Exception;

  /**
   * Notification on update
   *
   * @param bulkProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnUpdate(BulkProcess bulkProcess, String storeId) throws Exception;

  /**
   * Notification on update
   *
   * @param bulkProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnCampaignUpload(BulkProcess bulkProcess, String storeId)
      throws Exception;

  /**
   * Notification on instore update
   *
   * @param bulkProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnInstoreUpload(BulkProcess bulkProcess, String storeId) throws Exception;

  /**
   * process VAT update
   *
   * @param bulkUpdateEventModel
   * @throws Exception
   */
  void processEventForVatUpdate(BulkUpdateEventModel bulkUpdateEventModel) throws Exception ;

  /**
   * @param bulkProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndNotificationOnVatBulkUpdate(BulkProcess bulkProcess, String storeId) throws Exception;


  /**
   * send the final event to bp service after deleting pp code
   * @param storeId
   * @param bulkProcess
   * @param bulkProcessDataList
   */
  void sendFinalStatusEventForPickupPointDelete(String storeId, BulkProcess bulkProcess, List<BulkProcessData> bulkProcessDataList)
      throws Exception;

  /**
   * set Final Status And Notification For Work Order Upload
   * @param storeId not null
   * @param bulkProcess not null
   * @param bulkProcessDataList bulk process data list
   */
  void setFinalStatusAndNotificationForWorkOrderUpload(String storeId, BulkProcess bulkProcess,
    List<BulkProcessData> bulkProcessDataList) throws Exception;

  /**
   * set final status and notification on basic info update
   *
   * @param storeId
   * @param bulkProcess
   */
  void setFinalStatusAndNotificationOnBasicInfoUpdate(String storeId, BulkProcess bulkProcess)
      throws Exception;
}
