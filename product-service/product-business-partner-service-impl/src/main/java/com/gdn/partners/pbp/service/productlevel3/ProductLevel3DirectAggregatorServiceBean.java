package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorInventoryCriteria;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service("productLevel3DirectAggregatorService")
public class ProductLevel3DirectAggregatorServiceBean extends BaseProductLevel3AggregatorService
    implements ProductLevel3AggregatorServiceOld {

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;
  
  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Override
  public Page<ProductLevel3Summary> aggregateProductLevel3Summary(
      ProductLevel3SummaryFilter filterRequest, Pageable pageRequest, SortOrder sort)
      throws Exception {
    Page<ItemSummaryResponse> productDatas = new PageImpl<>(new ArrayList<>(), pageRequest, 0L);
    Map<String, List<CategoryResponse>> categoriesData;
    Map<String, PickupPointResponse> pickupPointDatas;
    Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
    ItemSummaryRequest itemFilterRequest =
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);

    // Filtering start from X-Inventory first
    if (filterRequest.getStock() != null) {
      List<ProductLevel3Inventory> inventoryList =
          productLevel3InventoryService.findInventoryByInventoryFilter(
              filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
              filterRequest.getInventoryFilter());
      inventoryDatas = modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList, filterRequest.getItemSkus());
      List<String> gdnSkus =
          modelConverter.convertProductLevel3InventoryToListOfGdnSku(inventoryList);
      if (CollectionUtils.isNotEmpty(gdnSkus)) {
        itemFilterRequest.setItemSkus(gdnSkus);
        productDatas =
            productLevel3Repository.findSummaryByFilter(itemFilterRequest, pageRequest, sort);
      }
    } else if (filterRequest.getInventoryFilter() != null) {
      Page<ProductLevel3Aggregator> productLevel3Aggregators =
          this.productLevel3AggregatorService
              .findByBusinessPartnerCodeAndInventoryFilterAndState(
                  filterRequest.getBusinessPartnerCode(),
                  ProductLevel3InventoryCriteria.STOCK_ALERT.equals(filterRequest.getInventoryFilter()) ? ProductLevel3AggregatorInventoryCriteria.MINIMUM_STOCK
                      : ProductLevel3AggregatorInventoryCriteria.valueOf(filterRequest.getInventoryFilter().name()),
                  filterRequest.getArchived() ? ProductLevel3AggregatorState.ARCHIVED
                      : ProductLevel3AggregatorState.ACTIVE, pageRequest);
      List<String> gdnSkus = new ArrayList<>();
      for (ProductLevel3Aggregator productLevel3Aggregator : productLevel3Aggregators) {
        if (Objects.nonNull(productLevel3Aggregator.getGdnSku()) && (
          CollectionUtils.isEmpty(filterRequest.getItemSkus()) || filterRequest.getItemSkus().contains(productLevel3Aggregator.getGdnSku()))) {
          gdnSkus.add(productLevel3Aggregator.getGdnSku());
        }
      }
      if (CollectionUtils.isNotEmpty(gdnSkus)) {
        itemFilterRequest.setItemSkus(gdnSkus);
        productDatas = new PageImpl<>(this.productLevel3Repository
            .findSummaryByFilter(itemFilterRequest, PageRequest.of(0, gdnSkus.size()), sort)
            .getContent(), pageRequest, productLevel3Aggregators.getTotalElements());
        List<ProductLevel3Inventory> inventories = this.productLevel3InventoryService
            .findInventoryByBusinessPartnerCodeAndListOfGdnSku(
                filterRequest.getBusinessPartnerCode(), gdnSkus);
        inventoryDatas =
            this.modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventories);
      }
    } else {
      productDatas =
          productLevel3Repository.findSummaryByFilter(itemFilterRequest, pageRequest, sort);
      List<String> gdnSkus =
          modelConverter.convertItemSummaryResponseToListOfGdnSku(productDatas.getContent());
      List<ProductLevel3Inventory> inventoryList =
          productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
              filterRequest.getBusinessPartnerCode(), gdnSkus);
      inventoryDatas = modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
    }

    categoriesData = super.generateProductCategoriesData(productDatas.getContent());
    pickupPointDatas = super.generateProductPickupPointsData(productDatas.getContent());
    return super.constructProductLevel3Summaries(productDatas, categoriesData, pickupPointDatas,
        inventoryDatas, pageRequest);
  }

  @Override
  public Page<ProductLevel3Summary> aggregateProductSummaryWithoutInventory(ProductLevel3SummaryFilter filterRequest,
    Pageable pageRequest) throws Exception {
    log.info("aggregating product level 3 summary without inventory details {} {}", filterRequest, pageRequest);
    ItemSummaryRequest itemFilterRequest = modelConverter
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);

    Page<ItemSummaryResponse> products = productLevel3Repository
      .findSummaryByFilter(itemFilterRequest, pageRequest, null);

    Map<String, List<CategoryResponse>> categories = super.generateProductCategoriesData(products.getContent());

    return super.constructProductLevel3Summaries(products, categories, pageRequest);
  }

  @Override
  public Page<ProductLevel3SummaryMinified> aggregateProductLevel3SummaryMinified(
      ProductLevel3SummaryFilter filterRequest, Pageable pageRequest, SortOrder sort)
      throws Exception {
    Page<ItemSummaryResponse> productDatas = new PageImpl<>(new ArrayList<>(), pageRequest, 0L);
    Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
    ItemSummaryRequest itemFilterRequest =
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);

    // Filtering start from X-Inventory first
    if (filterRequest.getStock() != null) {
      List<ProductLevel3Inventory> inventoryList =
          productLevel3InventoryService.findInventoryByInventoryFilter(
              filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
              filterRequest.getInventoryFilter());
      inventoryDatas = modelConverter
        .convertProductLevel3InventoryToMapOfGdnSku(inventoryList, filterRequest.getItemSkus());
      List<String> gdnSkus =
          modelConverter.convertProductLevel3InventoryToListOfGdnSku(inventoryList);
      if (CollectionUtils.isNotEmpty(gdnSkus)) {
        itemFilterRequest.setItemSkus(gdnSkus);
        productDatas =
            productLevel3Repository.findSummaryByFilter(itemFilterRequest, pageRequest, sort);
      }
    } else if (filterRequest.getInventoryFilter() != null) {
      Page<ProductLevel3Aggregator> productLevel3Aggregators =
          this.productLevel3AggregatorService
              .findByBusinessPartnerCodeAndInventoryFilterAndState(
                  filterRequest.getBusinessPartnerCode(),
                  ProductLevel3InventoryCriteria.STOCK_ALERT.equals(filterRequest.getInventoryFilter()) ? ProductLevel3AggregatorInventoryCriteria.MINIMUM_STOCK
                      : ProductLevel3AggregatorInventoryCriteria.valueOf(filterRequest.getInventoryFilter().name()),
                  filterRequest.getArchived() ? ProductLevel3AggregatorState.ARCHIVED
                      : ProductLevel3AggregatorState.ACTIVE, pageRequest);
      List<String> gdnSkus = new ArrayList<>();
      for (ProductLevel3Aggregator productLevel3Aggregator : productLevel3Aggregators) {
        gdnSkus.add(productLevel3Aggregator.getGdnSku());
      }
      gdnSkus.removeAll(Collections.singleton(null));
      if (CollectionUtils.isNotEmpty(gdnSkus)) {
        itemFilterRequest.setItemSkus(gdnSkus);
        productDatas = new PageImpl<>(this.productLevel3Repository
            .findSummaryByFilter(itemFilterRequest, PageRequest.of(0, gdnSkus.size()), sort)
            .getContent(), pageRequest, productLevel3Aggregators.getTotalElements());
        List<ProductLevel3Inventory> inventories = this.productLevel3InventoryService
            .findInventoryByBusinessPartnerCodeAndListOfGdnSku(
                filterRequest.getBusinessPartnerCode(), gdnSkus);
        inventoryDatas =
            this.modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventories);
      }
    } else {
      if (StringUtils.isNotEmpty(filterRequest.getGdnSku())) {
        List<String> filterGdnSkus = new ArrayList<>();
        filterGdnSkus.add(filterRequest.getGdnSku());
        itemFilterRequest.setItemSkus(filterGdnSkus);
      }
      productDatas =
          productLevel3Repository.findSummaryByFilter(itemFilterRequest, pageRequest, sort);
      List<String> gdnSkus =
          modelConverter.convertItemSummaryResponseToListOfGdnSku(productDatas.getContent());
      List<ProductLevel3Inventory> inventoryList =
          productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
              filterRequest.getBusinessPartnerCode(), gdnSkus);
      inventoryDatas = modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventoryList);
    }
    return super.constructProductLevel3SummariesMinified(productDatas, inventoryDatas, pageRequest);
  }

}