package com.gdn.mta.bulk.service;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.models.MasterWarehouseResponse;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkWorkOrderServiceWrapperImpl implements BulkWorkOrderServiceWrapper {

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private ProductAssemblyOutboundService productAssemblyOutboundService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private BulkWorkOrderService bulkWorkOrderService;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${master.warehouse.list.page}")
  private String page;

  @Value("${master.warehouse.list.limit}")
  private String limit;


  @Override
  public void processBulkWorkOrderUpload(WorkOrderEventModel workOrderEventModel) {
    String storeId = workOrderEventModel.getStoreId();
    BulkProcess bulkProcess =
        bulkProcessService.findByBulkProcessCode(storeId, workOrderEventModel.getBulkProcessCode());
    Optional<BulkProcessData> bulkProcessDataOptional =
        bulkProcessDataService.findBulkProcessDataById(workOrderEventModel.getId());
    BulkProcessData bulkProcessData = null;
    if (bulkProcessDataOptional.isEmpty()) {
      log.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
          workOrderEventModel.getBulkProcessCode(), workOrderEventModel.getRowNumber());
      return;
    }
    bulkProcessData = bulkProcessDataOptional.get();
    if (!BulkProcessData.STATUS_PENDING.equals(bulkProcessData.getStatus())) {
      log.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
          workOrderEventModel.getBulkProcessCode(), workOrderEventModel.getRowNumber());
      return;
    }
    try {
      bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
      bulkProcessData = bulkProcessDataService.saveOperation(bulkProcessData);
      WorkOrderDataModel workOrderDataModel =
          objectMapper.readValue(bulkProcessData.getBulkRequestData(), WorkOrderDataModel.class);
      ProfileResponse businessPartner =
          this.businessPartnerRepository.filterByBusinessPartnerCodeV2(storeId, bulkProcess.getBusinessPartnerCode());
      String merchantType =
          Optional.ofNullable(businessPartner.getCompany()).map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
      if (!Arrays.asList(productBundlingEligibleMerchantTypes.split(Constant.COMMA)).contains(merchantType)) {
        setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
            BulkWorkOrderConstants.NOT_ELIGIBLE_FOR_WORK_ORDER, null, 1);
        return;
      }
      String itemSku = workOrderDataModel.getSourceItemSku();
      String productSku = itemSku.substring(0, itemSku.lastIndexOf(Constant.HYPHEN));
      String warehouseCode = workOrderDataModel.getWarehouseCode();
      boolean isInternationalMerchant =
          BooleanUtils.toBooleanDefaultIfNull(bulkProcess.getInternationalMerchant(), false);
      BasicProductResponse productBasicDetails = xProductOutboundService.getBasicProductInfo(storeId, productSku);
      List<MasterWarehouseResponse> warehouseResponseList =
          productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(storeId, page, limit);
      if (warehouseResponseList.stream()
          .noneMatch(masterWarehouseResponse -> warehouseCode.equals(masterWarehouseResponse.getWarehouseCode()))) {
        setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
            BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(isInternationalMerchant), null, 1);
        return;
      }
      switch (bulkProcess.getBulkProcessType()) {
        case BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE:
        case BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE: {
          bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(storeId, merchantType, itemSku, productSku,
              warehouseCode, isInternationalMerchant, bulkProcess.getBulkProcessType(), bulkProcess, bulkProcessData,
              workOrderDataModel, productBasicDetails);
          break;
        }
        case BulkWorkOrderConstants.TRANSFER_BULK_PROCESS_TYPE:
          bulkWorkOrderService.validateAndProcessTransferRequest(storeId, merchantType, itemSku, productSku,
              warehouseCode, isInternationalMerchant, bulkProcess, bulkProcessData, workOrderDataModel,
              productBasicDetails);
          break;
      }
    } catch (Exception e) {
      log.error("Error while processing work order create event listener. workOrderEventModel : {} ",
          workOrderEventModel, e);
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL, Constant.SYSTEM_ERROR, 1,
          null);
    } finally {
      bulkProcessDataService.saveOperation(bulkProcessData);
    }
  }

  private void setBulkProcessDataStatusAndErrorMessage(BulkProcessData bulkProcessData, String status,
      String errorMessage, Integer systemErrorCount, Integer inputErrorCount) {
    bulkProcessData.setStatus(status);
    bulkProcessData.setInputErrorCount(inputErrorCount);
    bulkProcessData.setSystemErrorCount(systemErrorCount);
    bulkProcessData.setEndDate(new Date());
    bulkProcessData.setErrorMessage(errorMessage);
  }
}
