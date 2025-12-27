package com.gdn.mta.bulk.service;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.ChildSkuAndCogsMapping;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkWorkOrderServiceImpl implements BulkWorkOrderService {


  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private ProductAssemblyOutboundService productAssemblyOutboundService;

  public void validateAndProcessAssemblyDisassemblyRequest(String storeId, String merchantType, String itemSku,
      String productSku, String warehouseCode, boolean isInternationalMerchant, String workOrderType,
      BulkProcess bulkProcess, BulkProcessData bulkProcessData, WorkOrderDataModel workOrderDataModel,
      BasicProductResponse productBasicDetails) {
    List<ItemBasicDetailV2Response> itemBasicDetails = new ArrayList<>();
    if (isProductArchivedOrMarkedForDelete(productBasicDetails)) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.ITEM_SKU_INVALID.get(isInternationalMerchant), null, 1);
      return;
    }
    if (!productBasicDetails.isBundleProduct()) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.NOT_A_BUNDLE_PRODUCT.get(isInternationalMerchant), null, 1);
      return;
    }
    try {
      itemBasicDetails = xProductOutboundService.getItemBasicDetailByItemSku(storeId, true,
          new SimpleListStringRequest(Collections.singletonList(itemSku)));
      validateItem(isInternationalMerchant, itemBasicDetails.get(0));
    } catch (ApplicationRuntimeException e) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.ITEM_SKU_INVALID.get(isInternationalMerchant), null, 1);
      return;
    }
    ItemBasicDetailV2Response itemBasicDetailV2Response =
        itemBasicDetails.stream().findFirst().orElse(new ItemBasicDetailV2Response());
    Map<String, String> itemSkuCogsMap = new HashMap<>();
    itemSkuCogsMap =
        getItemSkuCogsMapForDisassemblyRequest(bulkProcessData, workOrderDataModel, isInternationalMerchant,
            workOrderType, itemBasicDetailV2Response, itemSkuCogsMap);
    if (BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE.equals(workOrderType) && MapUtils.isEmpty(
        itemSkuCogsMap)) {
      return;
    }
    SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest =
        RequestHelper.getSimpleListAssemblyDisassemblyRequest(productSku,
            Integer.parseInt(workOrderDataModel.getStock()), workOrderDataModel.getBundleType(),
            bulkProcess.getBusinessPartnerCode(), merchantType, warehouseCode, itemBasicDetailV2Response,
            itemSkuCogsMap);
    try {
      productAssemblyOutboundService.assemblyDisassemblyRequest(storeId, workOrderType, bulkProcess.getRequestId(),
          bulkProcess.getCreatedBy(), simpleListAssemblyDisassemblyRequest);
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_SUCCESS, StringUtils.EMPTY, null,
          null);
    } catch (Exception e) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL, e.getMessage(), null, 1);
    }
  }

  private Map<String, String> getItemSkuCogsMapForDisassemblyRequest(BulkProcessData bulkProcessData,
      WorkOrderDataModel workOrderDataModel, boolean isInternationalMerchant, String workOrderType,
      ItemBasicDetailV2Response itemBasicDetailV2Response, Map<String, String> itemSkuCogsMap) {
    if (BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE.equals(workOrderType)) {
      Set<String> bundleSkus =
          itemBasicDetailV2Response.getBundleRecipeList().stream().map(BundleRecipeV2Response::getItemSku)
              .collect(Collectors.toSet());
      List<ChildSkuAndCogsMapping> childSkuAndCogsMappingList = workOrderDataModel.getChildSkuAndCogsMapping();
      Set<String> childSkus =
          childSkuAndCogsMappingList.stream().map(ChildSkuAndCogsMapping::getItemSku).collect(Collectors.toSet());
      if (isChildSkuInvalid(itemBasicDetailV2Response, bundleSkus, childSkuAndCogsMappingList, childSkus)) {
        setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
            BulkWorkOrderConstants.CHILD_SKU_INVALID.get(isInternationalMerchant), null, 1);
        return null;
      }
      itemSkuCogsMap = childSkuAndCogsMappingList.stream().filter(Objects::nonNull)
          .collect(Collectors.toMap(ChildSkuAndCogsMapping::getItemSku, ChildSkuAndCogsMapping::getCogs));
    }
    return itemSkuCogsMap;
  }

  @Override
  public void validateAndProcessTransferRequest(String storeId, String merchantType, String sourceItemSku,
      String sourceProductSku, String warehouseCode, boolean isInternationalMerchant, BulkProcess bulkProcess,
      BulkProcessData bulkProcessData, WorkOrderDataModel workOrderDataModel,
      BasicProductResponse sourceProductBasicDetails) {
    String destinationItemSku = workOrderDataModel.getDestinationItemSku();
    String destinationProductSku = destinationItemSku.substring(0, destinationItemSku.lastIndexOf(Constant.HYPHEN));
    BasicProductResponse destinationProductBasicDetails =
        xProductOutboundService.getBasicProductInfo(storeId, destinationProductSku);
    //Validate source product
    if (isProductArchivedOrMarkedForDelete(sourceProductBasicDetails)) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.SOURCE_ITEM_INVALID.get(isInternationalMerchant), null, 1);
      return;
    }
    //Validate destination product
    if (isProductArchivedOrMarkedForDelete(destinationProductBasicDetails)) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(isInternationalMerchant), null, 1);
      return;
    }
    List<ItemBasicDetailV2Response> itemBasicDetails = new ArrayList<>();
    ItemBasicDetailV2Response sourceItemDetails = new ItemBasicDetailV2Response();
    ItemBasicDetailV2Response destinationItemDetails = new ItemBasicDetailV2Response();
    try {
      itemBasicDetails = xProductOutboundService.getItemBasicDetailByItemSku(storeId, true,
          new SimpleListStringRequest(Arrays.asList(sourceItemSku, destinationItemSku)));
      //Validate source and destination item sku
      if (validateSourceAndDestinationItemSku(bulkProcessData, sourceItemSku, isInternationalMerchant,
          destinationItemSku, itemBasicDetails)) {
        return;
      }
      Map<String, ItemBasicDetailV2Response> itemSkuItemBasicDetailMap = itemBasicDetails.stream()
          .collect(Collectors.toMap(ItemBasicDetailV2Response::getItemSku, Function.identity()));
      sourceItemDetails = itemSkuItemBasicDetailMap.get(sourceItemSku);
      destinationItemDetails = itemSkuItemBasicDetailMap.get(destinationItemSku);
    } catch (ApplicationRuntimeException e) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL, e.getErrorMessage(), null,
          1);
      return;
    }
    TransferRequest transferRequest =
        RequestHelper.getTransferRequest(sourceProductSku, destinationProductSku, sourceItemDetails,
            destinationItemDetails, bulkProcess.getBusinessPartnerCode(), merchantType, warehouseCode,
            workOrderDataModel);
    try {
      productAssemblyOutboundService.transferRequest(storeId, bulkProcess.getRequestId(), bulkProcess.getCreatedBy(),
          transferRequest);
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_SUCCESS, StringUtils.EMPTY, null,
          1);
    } catch (Exception e) {
      String errorMessage =
          StringUtils.replace(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), StringUtils.EMPTY);
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL, errorMessage, null, 1);
    }
  }

  public boolean validateSourceAndDestinationItemSku(BulkProcessData bulkProcessData, String sourceItemSku,
      boolean isInternationalMerchant, String destinationItemSku, List<ItemBasicDetailV2Response> itemBasicDetails) {
    Map<String, ItemBasicDetailV2Response> itemSkuItemBasicDetailMap =
        itemBasicDetails.stream().collect(Collectors.toMap(ItemBasicDetailV2Response::getItemSku, Function.identity()));
    if (Objects.isNull(itemSkuItemBasicDetailMap.get(sourceItemSku)) || itemSkuItemBasicDetailMap.get(sourceItemSku)
        .isMarkForDelete()) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.SOURCE_ITEM_INVALID.get(isInternationalMerchant), null, 1);
      return true;
    }
    if (Objects.isNull(itemSkuItemBasicDetailMap.get(destinationItemSku)) || itemSkuItemBasicDetailMap.get(
        destinationItemSku).isMarkForDelete()) {
      setBulkProcessDataStatusAndErrorMessage(bulkProcessData, BulkProcessData.STATUS_FAIL,
          BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(isInternationalMerchant), null, 1);
      return true;
    }
    return false;
  }

  private static boolean isProductArchivedOrMarkedForDelete(BasicProductResponse productBasicDetails) {
    return productBasicDetails.isArchived() || productBasicDetails.isMarkForDelete();
  }

  public static boolean isChildSkuInvalid(ItemBasicDetailV2Response itemBasicDetailV2Response, Set<String> bundleSkus,
      List<ChildSkuAndCogsMapping> childSkuAndCogsMappingList, Set<String> childSkus) {
    return !(Objects.equals(childSkuAndCogsMappingList.size(), itemBasicDetailV2Response.getBundleRecipeList().size())
        && childSkus.containsAll(bundleSkus));
  }


  private static void validateItem(boolean isInternationalMerchant, ItemBasicDetailV2Response itemBasicDetails) {
    if (itemBasicDetails.isMarkForDelete()) {
      log.error("Error while creating assembly/disassembly request , itemSku : {} is deleted. ",
          itemBasicDetails.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          BulkWorkOrderConstants.ITEM_SKU_INVALID.get(isInternationalMerchant));
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
