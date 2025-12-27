package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;


import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.StockStatus;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.PickupPointRepository;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointInventoryService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.InventoryType;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomInventoryInfoRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomInventoryPickupPointInfoRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component
@Slf4j
public class CustomInventoryServiceV2 {

  @Autowired
  private CustomInventoryPickupPointInfoRepository customInventoryPickupPointInfoRepository;

  @Autowired
  private CustomInventoryInfoRepository customInventoryInfoRepository;

  @Autowired
  private PickupPointInventoryService pickupPointInventoryService;

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Value("${skip.stock.fetch.from.inventory.module}")
  private boolean skipStockFetchFromInventoryModule;

  public List<CustomInventoryPickupPointInfo> findAllCustomInventoryPickupPointInfoByItemSkus(Set<String> itemSkus) {
    if(skipStockFetchFromInventoryModule){
      return Collections.emptyList();
    }
    return customInventoryPickupPointInfoRepository.findByItemSkuIn(itemSkus);
  }

  public List<CustomInventoryInfo> findAllCustomInventoryInfoByProductSku(Set<String> itemSkus) {
    if(skipStockFetchFromInventoryModule){
      return Collections.emptyList();
    }
    return customInventoryInfoRepository.findByItemSkuIn(itemSkus);
  }

  public Stock getStockByL5(String itemSku, String pickupPointCode,
    List<CustomInventoryPickupPointInfo> allCustomInventoryPickupPointInfos,
    List<CustomInventoryInfo> allCustomInventoryInfo, PickupPoint pickupPoint) {
    Stock stock = getStockByL5FromInventoryPickupPointInfo(itemSku, pickupPointCode,
      allCustomInventoryPickupPointInfos, skipStockFetchFromInventoryModule);
    if (Objects.nonNull(stock)) {
      return stock;
    }
    if (skipStockFetchFromInventoryModule) {
      return ModuleProductUtil.initializeStockData(itemSku, pickupPointCode, pickupPoint);
    }
    return getStockByL5FromInventoryInfo(itemSku, pickupPointCode, allCustomInventoryInfo);
  }


  private Stock getStockByL5FromInventoryPickupPointInfo(String itemSku, String pickupPointCode,
    List<CustomInventoryPickupPointInfo> allCustomInventoryPickupPointInfos,
    boolean skipStockFetchFromInventoryModule) {
    if (StringUtils.isAnyEmpty(itemSku, pickupPointCode)) {
      return null;
    }

    String pickupPointId = ModuleProductUtil.toPickupPointId(itemSku, pickupPointCode);
    List<CustomInventoryPickupPointInfo> inventoryList =
      Optional.ofNullable(allCustomInventoryPickupPointInfos).orElseGet(ArrayList::new);
    if(skipStockFetchFromInventoryModule){
      PickupPointInventory pickupPointInventory = Optional.ofNullable(
        pickupPointInventoryService.getExistingPickupPointInventory(pickupPointId)).orElseGet(
        () -> ModuleProductUtil.toPickupPointInventoryFromPickupPoint(
          pickupPointRepository.findById(pickupPointId)));
      if(Objects.nonNull(pickupPointInventory)){
        return ModuleProductUtil.buildBasicStock(itemSku, pickupPointCode, pickupPointInventory);
      }
      return null;
    }
    return fetchInventoryInfo(inventoryList, pickupPointId, skipStockFetchFromInventoryModule).map(
      inventoryInfo -> skipStockFetchFromInventoryModule ?
        buildBasicStock(itemSku, pickupPointCode, inventoryInfo) :
        buildDetailedStock(itemSku, pickupPointCode, inventoryInfo)).orElse(null);
  }

  private Optional<CustomInventoryPickupPointInfo> fetchInventoryInfo(
    List<CustomInventoryPickupPointInfo> inventoryList, String pickupPointId,
    boolean skipStockFetchFromInventoryModule) {

    String ppId = Optional.ofNullable(pickupPointId).orElse(StringUtils.EMPTY);

    Optional<CustomInventoryPickupPointInfo> result;
    if (CollectionUtils.isEmpty(inventoryList)) {
      if (skipStockFetchFromInventoryModule) {
        result = Optional.empty();
      } else {
        result = customInventoryPickupPointInfoRepository.findById(ppId)
          .filter(Predicate.not(CustomInventoryPickupPointInfo::isMarkForDelete));
      }
    } else {
      result = inventoryList.stream().filter(info -> Objects.equals(ppId, info.getId()))
        .filter(Predicate.not(BaseData::isMarkForDelete)).findFirst();
    }

    return result;
  }


  private Stock buildBasicStock(String itemSku, String pickupPointCode,
    CustomInventoryPickupPointInfo inventoryInfo) {
    // with skipStockFetchFromInventoryModule, we only care about status
    return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode)
      .warehouse(ModuleProductUtil.isWarehouse(ModuleProductUtil.toPickupPointType(inventoryInfo)))
      .status(inventoryInfo.getAvailableStock() > StockStatus.QTY_OOS ?
        StockStatus.AVAILABLE : StockStatus.OOS).exists(true).build();
  }

  private Stock buildDetailedStock(String itemSku, String pickupPointCode,
    CustomInventoryPickupPointInfo inventoryInfo) {
    // with switch off skipStockFetchFromInventoryModule, we care about full stock details
    return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode)
      .quota(inventoryInfo.getOriginalStock()).remaining(inventoryInfo.getAvailableStock())
      .warehouse(ModuleProductUtil.isWarehouse(ModuleProductUtil.toPickupPointType(inventoryInfo)))
      .status(ModuleProductUtil.convertStockStatus(inventoryInfo.getAvailableStock())).exists(true)
      .build();
  }

  private Stock getStockByL5FromInventoryInfo(String itemSku, String pickupPointCode,
      List<CustomInventoryInfo> allCustomInventoryInfo) {
    return Optional.ofNullable(allCustomInventoryInfo).orElseGet(ArrayList::new).stream().filter(
            customInventoryInfo -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY)
                .equals(customInventoryInfo.getItemSku()))
        .filter(customInventoryInfo -> InventoryType.TYPE_ONLINE_MERCHANT.equals(customInventoryInfo.getType()))
        .filter(inventoryInfo -> CollectionUtils.isNotEmpty(inventoryInfo.getStockInformations()))
        .map(inventoryInfo -> {
          List<CustomInventoryInfo.StockInformation> stockInformations = inventoryInfo.getStockInformations();
          Stock result = ModuleProductUtil.initializeStock(itemSku, pickupPointCode);
          result.setItemSku(itemSku);
          result.setPickupPointCode(pickupPointCode);
          setStockStatusAndQuantity(pickupPointCode, stockInformations, result);
          result.setWarehouse(ModuleProductUtil.isWarehouse(inventoryInfo.getType()));
          result.setExists(true);
          return result;
        }).findFirst().orElseGet(() -> ModuleProductUtil.initializeStock(itemSku, pickupPointCode));
  }

  private void setStockStatusAndQuantity(String pickupPointCode,
    List<CustomInventoryInfo.StockInformation> stockInformation, Stock result) {
    if(!skipStockFetchFromInventoryModule) {
      fetchStockQuantityFromInventoryModule(pickupPointCode, stockInformation, result);
    }
    else {
      // with skipStockFetchFromInventoryModule, we only care about status
      int totalAvailableStock = stockInformation.stream().filter(Objects::nonNull)
        .map(CustomInventoryInfo.StockInformation::getStockInformationDetails).filter(CollectionUtils::isNotEmpty)
        .flatMap(Collection::stream).filter(Objects::nonNull)
        .filter(val -> ModuleProductUtil.isPickupPointCodeSame(pickupPointCode, val.getPickupPointCode()))
        .map(CustomInventoryInfo.StockInformationDetail::getAvailableStock).mapToInt(Integer::intValue).sum();
      result.setStatus(totalAvailableStock > StockStatus.QTY_OOS ?
        StockStatus.AVAILABLE : StockStatus.OOS);
    }
  }

  private static void fetchStockQuantityFromInventoryModule(String pickupPointCode,
    List<CustomInventoryInfo.StockInformation> stockInformations, Stock result) {
    int totalOriginalStock = stockInformations.stream().filter(Objects::nonNull)
      .map(CustomInventoryInfo.StockInformation::getStockInformationDetails).filter(CollectionUtils::isNotEmpty)
      .flatMap(Collection::stream).filter(Objects::nonNull)
      .filter(val -> ModuleProductUtil.isPickupPointCodeSame(pickupPointCode, val.getPickupPointCode()))
      .map(CustomInventoryInfo.StockInformationDetail::getOriginalStock).mapToInt(Integer::intValue).sum();
    int totalAvailableStock = stockInformations.stream().filter(Objects::nonNull)
      .map(CustomInventoryInfo.StockInformation::getStockInformationDetails).filter(CollectionUtils::isNotEmpty)
      .flatMap(Collection::stream).filter(Objects::nonNull)
      .filter(val -> ModuleProductUtil.isPickupPointCodeSame(pickupPointCode, val.getPickupPointCode()))
      .map(CustomInventoryInfo.StockInformationDetail::getAvailableStock).mapToInt(Integer::intValue).sum();
    result.setQuota(result.getQuota() + totalOriginalStock);
    result.setRemaining(result.getRemaining() + totalAvailableStock);
    result.setStatus(ModuleProductUtil.convertStockStatus(result.getRemaining()));
  }
}
