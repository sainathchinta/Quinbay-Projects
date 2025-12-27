package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.service.util.ConverterUtil.constructQrCodeBulkProcessData;
import static com.gdn.mta.bulk.service.util.ConverterUtil.constructQrCodeRowInfo;
import static com.gdn.mta.bulk.service.util.ConverterUtil.constructQrCodeRowItemInfo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.QRCodeGenerationUtil;
import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.ProductDetailsRequest;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.RowNumberParentCodeDTO;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.repository.BulkProcessDataRepository;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Service
@Transactional(readOnly = true)
@Slf4j
public class BulkProcessDataServiceBean implements BulkProcessDataService {

  @Autowired
  private BulkProcessDataRepository bulkProcessDataRepository;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Value("${fetch.item.pickup.point.size}")
  private int fetchItemPickupPointSize;

  @Override
  public List<BulkProcessData> findByStoreIdAndBulkProcess(String storeId, BulkProcess bulkProcess) {
    return bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId,
        bulkProcess.getBulkProcessCode());
  }


  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public BulkProcessData saveOperation(BulkProcessData bulkProcessData) {
    BulkProcessData savedBulkProcessData = bulkProcessDataRepository.findById(bulkProcessData.getId()).get();
    BeanUtils.copyProperties(bulkProcessData, savedBulkProcessData);
    return this.bulkProcessDataRepository.save(savedBulkProcessData);
  }

  @Override
  public List<BulkProcessData> findByBulkProcessCodeAndParentProductAndStatus(String storeId, BulkProcess bulkProcess,
      String parentProduct, String status) throws Exception {
    return bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndParentProductAndStatusAndMarkForDeleteFalse(
        storeId, bulkProcess.getBulkProcessCode(), parentProduct, status);
  }

  @Override
  public List<BulkProcessData> findByBulkProcessIdAndParentProductAndStatus(String storeId,
      String bulkProcessId, String parentProduct, String status) throws Exception {
    return bulkProcessDataRepository.findByStoreIdAndBulkProcessIdAndParentProductAndStatusAndMarkForDeleteFalse(
        storeId, bulkProcessId, parentProduct, status);
  }

  @Override
  public List<BulkProcessData> getFailedDataForProcessedFile(String storeId, String bulkProcessCode) throws Exception {
    return bulkProcessDataRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(storeId, bulkProcessCode,
            BulkProcessData.STATUS_FAIL);
  }

  @Override
  public List<BulkProcessData> getFailedDataForProcessedFileInStatusIn(String storeId,
      String bulkProcessCode,
      List<String> statusList) throws Exception {
    return bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndStatusInAndMarkForDeleteFalse(
        storeId, bulkProcessCode, statusList);
  }

  @Override
  public List<String> getDistinctParentProduct(String storeId, String bulkProcessCode) throws Exception {
    return bulkProcessDataRepository
        .getDistinctParentForBlpCode(storeId, bulkProcessCode, BulkProcessData.STATUS_PENDING);
  }

  @Override
  public List<Integer> findRowNumberByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode,
      String status) {
    return bulkProcessDataRepository
        .findRowNumberByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode, status);
  }

  @Override
  public List<RowNumberParentCodeDTO> getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(String storeId,
      String bulkProcessCode, String status) {
    return bulkProcessDataRepository
        .getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode, status);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveBulkProcessData(List<BulkProcessData> bulkProcessDataList) {
    bulkProcessDataRepository.saveAll(bulkProcessDataList);
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
  public void saveOperationBulkProcessData(List<BulkProcessData> bulkProcessDataList) {
    bulkProcessDataRepository.saveAll(bulkProcessDataList);
  }

  @Override
  @Transactional(readOnly = false)
  public List<BulkProcessData> saveAndReturnBulkProcessData(List<BulkProcessData> bulkProcessDataList) {
    return bulkProcessDataRepository.saveAll(bulkProcessDataList);
  }

  @Override
  public void deleteDataByBulkProcessCode(String storeId, String bulkProcessCode) {
    bulkProcessDataRepository.deleteByBulkProcessCode(storeId, bulkProcessCode);
  }

  @Override
  @Transactional(readOnly = false)
  public void updatePendingProcesses(String storeId, List<String> bulkProcessCodes, Date updatedDate) {
    log.info("Updating the status in blp_bulk_process_data for bulk process codes : {} and updatedDate : {}",
        bulkProcessCodes, updatedDate);
    bulkProcessDataRepository.updatePendingProcesses(storeId, bulkProcessCodes, updatedDate, Constant.SYSTEM_ERROR);
  }

  @Override
  public List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(String storeId, String bulkProcessCode,
      List<Integer> rowNumbers, String status) throws Exception {
    return bulkProcessDataRepository
        .findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(storeId, bulkProcessCode, rowNumbers, status);
  }

  @Override
  public Page<BulkProcessData> findByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode,
      String status, Pageable pageable) throws Exception {
    return bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode, status,
        pageable);
  }

  @Override
  public List<String> getPendingBulkProcessCodes(String storeId, List<String> bulkProcessCodes) throws Exception{
    return bulkProcessDataRepository.getPendingBulkProcessCodes(storeId, bulkProcessCodes);
  }

  @Override
  @Transactional
  public void abortPendingBulkProcessDataBeforeOrById(String storeId, String id) {
    int abortPendingBulkProcessLimit = Integer.parseInt(
      systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT).getValue());
    Date pendingToAbortDate = ProcessorUtils.getEarlierDateBySeconds(abortPendingBulkProcessLimit);
    if (StringUtils.isEmpty(id)) {
      log.info("Aborting all pending tasks for storeId {} before {}", storeId, pendingToAbortDate);
      bulkProcessDataRepository.updateStatusInPendingOrInProgressBulkProcessToAborted(
        pendingToAbortDate);
    } else {
      log.info("Aborting all pending tasks for storeId {} before {} and id {}", storeId,
        pendingToAbortDate, id);
      bulkProcessDataRepository.updateBulkProcessDataStatusToFailById(id);
    }
  }

  @Override
  @Transactional(readOnly = false, noRollbackFor = Exception.class)
  public void saveRequestInBulkProcessData(DownloadQRCodeRequest downloadQRCodeRequest,
      BulkProcess bulkProcess) throws Exception {
    log.info("Generating bulk process data for request : {} , bulkProcess : {}", downloadQRCodeRequest, bulkProcess);
    if (downloadQRCodeRequest.getQrPerPage().equals(0)) {
      log.error("#DownloadQRCodeListener saveRequestInBulkProcessData received request: {} with 0 items per page",
          downloadQRCodeRequest);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "Data not found from xbp call");
    }
    AllowedQRGenerationType qrGenerationType =
        AllowedQRGenerationType.valueOf(downloadQRCodeRequest.getQrGenerationType().toUpperCase());
    List<ProductDetailsRequest> itemDetailsRequestList = new ArrayList<>();
    switch (qrGenerationType.getValue()) {
      case Constant.STORE:
        if (downloadQRCodeRequest.isAllStores()) {
          List<ProductDetailsRequest> list = new ArrayList<>();
          List<PickupPointResponse> pickupPointResponsesList = pickupPointService.getPickupPointSummaryFilter(0,
              PickupPointFilterRequest.builder().businessPartnerCode(downloadQRCodeRequest.getMerchantCode()).build());
          if (CollectionUtils.isEmpty(pickupPointResponsesList)) {
            bulkProcess.setTotalCount(1);
            bulkProcess.setErrorCount(1);
            throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "Data not found from xbp call");
          } else {
            pickupPointResponsesList.stream().map(pickupPointResponse -> list.add(
                ProductDetailsRequest.builder().pickupPointCode(pickupPointResponse.getCode())
                    .pickupPointName(pickupPointResponse.getName()).build())).collect(Collectors.toList());
            downloadQRCodeRequest.setProductDetailsRequestList(list);
          }
        } else {
          if (CollectionUtils.isEmpty(downloadQRCodeRequest.getProductDetailsRequestList())) {
            ProductDetailsRequest productDetailsRequest = new ProductDetailsRequest();
            downloadQRCodeRequest.setProductDetailsRequestList(Collections.singletonList(productDetailsRequest));
          }
        }
        break;
      case Constant.PRODUCT:
        if (CollectionUtils.isNotEmpty(downloadQRCodeRequest.getProductDetailsRequestList())) {
          for (ProductDetailsRequest productDetailsRequest : downloadQRCodeRequest.getProductDetailsRequestList()) {
            if (StringUtils.isNotBlank(productDetailsRequest.getProductSku())) {
              itemDetailsRequestList.add(
                  ProductDetailsRequest.builder().productSku(productDetailsRequest.getProductSku())
                      .itemName(productDetailsRequest.getItemName()).build());
            }
          }
          downloadQRCodeRequest.setProductDetailsRequestList(itemDetailsRequestList);
        }
        break;
      case Constant.ITEM:
        if (CollectionUtils.isNotEmpty(downloadQRCodeRequest.getProductDetailsRequestList())) {
          for (ProductDetailsRequest productDetailsRequest : downloadQRCodeRequest.getProductDetailsRequestList()) {
            if (StringUtils.isNotBlank(productDetailsRequest.getProductSku())) {
              List<ItemBasicDetailV2Response> itemResponseList =
                  xProductOutboundService.getItemBasicDetails(productDetailsRequest.getProductSku());
              if (CollectionUtils.isNotEmpty(itemResponseList)) {
                itemResponseList.stream().map(itemBasicDetailV2Response -> itemDetailsRequestList.add(
                        ProductDetailsRequest.builder().itemSku(itemBasicDetailV2Response.getItemSku())
                            .itemName(itemBasicDetailV2Response.getGeneratedItemName()).build()))
                    .collect(Collectors.toList());
              } else {
                log.error("Item Data not found for request in xproduct : {} ", downloadQRCodeRequest);
                throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "Data not found for product Sku");
              }
            } else if (StringUtils.isNotBlank(productDetailsRequest.getItemSku())) {
              itemDetailsRequestList.add(ProductDetailsRequest.builder().itemSku(productDetailsRequest.getItemSku())
                  .itemName(productDetailsRequest.getItemName()).build());
            }
          }
          downloadQRCodeRequest.setProductDetailsRequestList(itemDetailsRequestList);
        }
        break;
      case Constant.ITEM_PICKUP_POINT:
      case Constant.ADD_TO_BAG:
        if (CollectionUtils.isNotEmpty(downloadQRCodeRequest.getProductDetailsRequestList())) {
          for (ProductDetailsRequest productDetailsRequest : downloadQRCodeRequest.getProductDetailsRequestList()) {
            if (StringUtils.isNotBlank(productDetailsRequest.getProductSku())) {
              int page = 0;
              Page<ItemL5ListingResponse> itemL5ListingResponsePage;
              do {
                itemL5ListingResponsePage =
                    this.xProductOutboundService.getItemL5Details(productDetailsRequest.getProductSku(),
                        downloadQRCodeRequest.isCncActivated(), page, fetchItemPickupPointSize);
                itemL5ListingResponsePage.getContent().stream().map(itemL5ListingResponse -> itemDetailsRequestList.add(
                        ProductDetailsRequest.builder().itemSku(itemL5ListingResponse.getItemSku())
                            .itemName(itemL5ListingResponse.getItemName())
                            .pickupPointCode(itemL5ListingResponse.getPpCode())
                            .pickupPointName(itemL5ListingResponse.getPpCodeName())
                            .productPrice(
                                QRCodeGenerationUtil.convertPriceToIndonesianFormat(
                                    itemL5ListingResponse.getOfferPrice()))
                            .build()))
                    .collect(Collectors.toList());
                page++;
              } while (itemL5ListingResponsePage.hasNext());
            } else if (StringUtils.isNotBlank(productDetailsRequest.getItemSku()) && StringUtils.isNotBlank(
                productDetailsRequest.getPickupPointCode())) {
              ProductDetailsRequest productDetails = new ProductDetailsRequest();
              productDetails.setItemSku(productDetailsRequest.getItemSku());
              productDetails.setItemName(productDetailsRequest.getItemName());
              productDetails.setPickupPointCode(productDetailsRequest.getPickupPointCode());
              productDetails.setPickupPointName(productDetailsRequest.getPickupPointName());
              if(StringUtils.isNotBlank(productDetailsRequest.getProductPrice())) {
                productDetails.setProductPrice(QRCodeGenerationUtil.convertPriceToIndonesianFormat(
                    Double.parseDouble(productDetailsRequest.getProductPrice())));
              }
              itemDetailsRequestList.add(productDetails);
            }
          }
          downloadQRCodeRequest.setProductDetailsRequestList(itemDetailsRequestList);
        }
        break;
      case Constant.ALL_PRODUCTS:
        ProductDetailsRequest productDetailsRequest = new ProductDetailsRequest();
        productDetailsRequest.setPickupPointCode(Constant.ALL_LOCATIONS);
        productDetailsRequest.setPickupPointName(Constant.LIHAT_SEMUA_PRODUK);
        downloadQRCodeRequest.setProductDetailsRequestList(
            Collections.singletonList(productDetailsRequest));
        break;
      default:
        break;
    }

    if (CollectionUtils.isEmpty(downloadQRCodeRequest.getProductDetailsRequestList())) {
      log.error("product list is empty for the request : {} ", downloadQRCodeRequest);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Product list cannot be empty");
    }
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    QrCodeRowInfo qrCodeRowInfo = constructQrCodeRowInfo(downloadQRCodeRequest);
    List<List<ProductDetailsRequest>> partitionList =
        Lists.partition(downloadQRCodeRequest.getProductDetailsRequestList(), downloadQRCodeRequest.getQrPerPage());
    for (List<ProductDetailsRequest> productDetailsRequestList : partitionList) {
      qrCodeRowInfo.setRowItems(
          constructQrCodeRowItemInfo(productDetailsRequestList, downloadQRCodeRequest.isCncActivated()));
      bulkProcessDataList.add(
          constructQrCodeBulkProcessData(qrCodeRowInfo, (partitionList.indexOf(productDetailsRequestList) + 1),
              bulkProcess));
    }
    bulkProcess.setTotalCount(partitionList.size());
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    saveBulkProcessData(bulkProcessDataList);
  }

  @Override
  public List<BulkProcessDataDTO> getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(
    String storeId, String bulkProcessCode, String status) {
    return bulkProcessDataRepository
      .getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(storeId, bulkProcessCode, status);
  }

  @Override
  public Optional<BulkProcessData> findBulkProcessDataById(String id) {
    return bulkProcessDataRepository.findById(id);
  }

  @Override
  public void updateStatusToFailByBulkProcessCodeAndStatusIn(String bulkProcessCode,
    List<String> statuses) {
    bulkProcessDataRepository.updateStatusToFailByBulkProcessCodeAndStatusIn(bulkProcessCode, statuses);
  }

  @Override
  public Map<String, List<BulkProcessData>> findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(String storeId,
    String bulkProcessCode, String status) {
    List<BulkProcessData> bulkProcessDataList =
      bulkProcessDataRepository.findByStoreIdAndBulkProcessCodeAndStatusAndIdentifierNotNull(
        storeId, bulkProcessCode, status);
    bulkProcessDataList.forEach(bulkProcessData -> bulkProcessData.setParentProduct(
      BulkUpdateServiceUtil.extractProductSku(bulkProcessData.getParentProduct())));
    return bulkProcessDataList.stream()
      .collect(Collectors.groupingBy(BulkProcessData::getParentProduct));
  }
}
