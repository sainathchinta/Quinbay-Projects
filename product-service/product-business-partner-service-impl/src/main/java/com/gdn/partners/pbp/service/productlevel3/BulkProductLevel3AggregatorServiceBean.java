package com.gdn.partners.pbp.service.productlevel3;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.product.util.CommonUtils;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.google.common.collect.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;

import lombok.extern.slf4j.Slf4j;


@Slf4j
@Service("bulkProductLevel3AggregatorServiceBean")
public class BulkProductLevel3AggregatorServiceBean implements BulkProductLevel3AgrregatorService {
  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private ProductLevel3Converter modelConverter;

  @Value("${product.fetch.maximum.size}")
  private String productFetchMaximumSize;

  @Override
  public BulkDownloadProductLevel3Response aggregateProductLevel3SummaryByDb(
    ProductLevel3SummaryFilter filterRequest, boolean fetchB2bData, String fetchViewConfigByChannel) throws Exception {
    int productBatchSize = Integer.parseInt(productFetchMaximumSize);
    Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
    List<ItemLevel5Response> productL5Data =
        xProductOutbound.getL5ItemListing(new HashSet<>(filterRequest.getProductSkuList()),
            filterRequest.getPickupPointCodes(), filterRequest.getPromoTypes(), fetchB2bData,
            fetchViewConfigByChannel);
    List<List<ItemLevel5Response>> level5PartitionList = Lists.partition(productL5Data,
      productBatchSize);
    for(List<ItemLevel5Response> itemLevel5RequestList : level5PartitionList){
      Map<String, ProductLevel3Inventory> inventoryL5Data;
      List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList =
        itemLevel5RequestList.stream().map(CommonUtils::toInvDetailInfoRequest).collect(Collectors.toList());
      List<ProductLevel3Inventory> inventoryList = productLevel3InventoryService
        .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(inventoryDetailInfoRequestDTOList);
        inventoryL5Data =
          inventoryList.stream().collect(Collectors.toMap(
            productLevel3Inventory -> com.gdn.partners.pbp.commons.util.CommonUtils
              .getItemSkuAndPickupPointKey(productLevel3Inventory.getWebItemSku(),
                productLevel3Inventory.getWebPickupPointCode()), Function.identity(), (v1, v2) -> v2));

      inventoryDatas.putAll(inventoryL5Data);
    }
    
    return ConverterUtil.constructItemL5ListingResponse(productL5Data, inventoryDatas);
  }
}
