package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.repository.mv.MerchantProductMVRepository;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

@Service("productLevel3MaterializedViewAggregatorService")
public class ProductLevel3MaterializedViewAggregatorServiceBean extends
    BaseProductLevel3AggregatorService implements ProductLevel3AggregatorServiceOld {

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private MerchantProductMVRepository merchantProductMVRepository;

  @Override
  public Page<ProductLevel3Summary> aggregateProductLevel3Summary(
      ProductLevel3SummaryFilter filterRequest, Pageable pageRequest, SortOrder sort)
      throws Exception {
    Page<MerchantProductMV> pageOfMerchantProductMV =
        merchantProductMVRepository.findByFilter(filterRequest, pageRequest, sort);
    List<String> itemSkus = new ArrayList<>();
    for (MerchantProductMV merchantProductMV : pageOfMerchantProductMV.getContent()) {
      itemSkus.add(merchantProductMV.getItemSku());
    }

    ItemSummaryRequest filterToGetItems = new ItemSummaryRequest();
    filterToGetItems.setMerchantCode(filterRequest.getBusinessPartnerCode());
    filterToGetItems.setItemSkus(itemSkus);
    Page<ItemSummaryResponse> pageOfItems =
        productLevel3Repository.findSummaryByFilter(filterToGetItems, pageRequest, sort);
    List<ProductLevel3Inventory> inventories =
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), itemSkus);
    Map<String, ProductLevel3Inventory> mapOfProductInventoryDatas =
        modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Map<String, List<CategoryResponse>> mapOfProductCategoryDatas =
        super.generateProductCategoriesData(pageOfItems.getContent());
    Map<String, PickupPointResponse> mapOfProductPickupPointDatas =
        super.generateProductPickupPointsData(pageOfItems.getContent());

    return super.constructProductLevel3Summaries(pageOfItems, mapOfProductCategoryDatas,
        mapOfProductPickupPointDatas, mapOfProductInventoryDatas, pageRequest);
  }

  @Override
  public Page<ProductLevel3SummaryMinified> aggregateProductLevel3SummaryMinified(
      ProductLevel3SummaryFilter filterRequest, Pageable pageRequest, SortOrder sort)
      throws Exception {
    Page<MerchantProductMV> pageOfMerchantProductMV =
        merchantProductMVRepository.findByFilter(filterRequest, pageRequest, sort);
    List<String> itemSkus = new ArrayList<>();
    for (MerchantProductMV merchantProductMV : pageOfMerchantProductMV.getContent()) {
      itemSkus.add(merchantProductMV.getItemSku());
    }

    ItemSummaryRequest filterToGetItems = new ItemSummaryRequest();
    filterToGetItems.setMerchantCode(filterRequest.getBusinessPartnerCode());
    filterToGetItems.setItemSkus(itemSkus);
    Page<ItemSummaryResponse> pageOfItems =
        productLevel3Repository.findSummaryByFilter(filterToGetItems, pageRequest, sort);
    List<ProductLevel3Inventory> inventories =
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), itemSkus);
    Map<String, ProductLevel3Inventory> mapOfProductInventoryDatas =
        modelConverter.convertProductLevel3InventoryToMapOfGdnSku(inventories);
    return super.constructProductLevel3SummariesMinified(pageOfItems, mapOfProductInventoryDatas,
        pageRequest);
  }

  @Override
  public Page<ProductLevel3Summary> aggregateProductSummaryWithoutInventory(ProductLevel3SummaryFilter filterRequest,
    Pageable pageRequest) throws Exception {
    Page<MerchantProductMV> pageOfMerchantProductMV =
      merchantProductMVRepository.findByFilter(filterRequest, pageRequest, null);
    List<String> itemSkus = new ArrayList<>();
    for (MerchantProductMV merchantProductMV : pageOfMerchantProductMV.getContent()) {
      itemSkus.add(merchantProductMV.getItemSku());
    }

    ItemSummaryRequest filterToGetItems = new ItemSummaryRequest();
    filterToGetItems.setMerchantCode(filterRequest.getBusinessPartnerCode());
    filterToGetItems.setItemSkus(itemSkus);
    Page<ItemSummaryResponse> pageOfItems =
      productLevel3Repository.findSummaryByFilter(filterToGetItems, pageRequest, null);

    Map<String, List<CategoryResponse>> mapOfProductCategoryDatas =
      super.generateProductCategoriesData(pageOfItems.getContent());

    return super.constructProductLevel3Summaries(pageOfItems, mapOfProductCategoryDatas, pageRequest);
  }

}
