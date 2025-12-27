package com.gdn.partners.pbp.service.productlevel3;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetailsImage;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;

public interface ProductLevel3Converter {

  ProductLevel3Inventory convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
      InventoryDetailInfoResponseV2DTO source);

  ProductLevel3Inventory convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
      InventoryDetailInfoResponseDTO source);

  List<String> convertItemSummaryResponseToListOfGdnSku(List<ItemSummaryResponse> source);

  /**
   * Convert to ItemSkuAndPickupCodeMap
   *
   * @param source
   * @return
   */
  Map<String, String> convertItemSummaryResponseToItemSkuAndPickupCodeMap(List<ItemSummaryResponse> source);

  List<String> convertProductLevel3InventoryToListOfGdnSku(List<ProductLevel3Inventory> source);

  Map<String, ProductLevel3Inventory> convertProductLevel3InventoryToMapOfGdnSku(List<ProductLevel3Inventory> source);

  Map<String, ProductLevel3Inventory> convertProductLevel3InventoryToMapOfGdnSku(List<ProductLevel3Inventory> source,
      List<String> filter);

  ItemSummaryRequest convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
      ProductLevel3SummaryFilter filterRequest);

  List<ProductLevel3Price> convertItemPricesToProductLevel3Prices(Collection<PriceDTO> itemPrices);

  List<ProductLevel3ViewConfig> convertItemViewConfigsToProductLevel3ViewConfigs(
      Collection<ItemViewConfigDTO> itemViewConfigs);

  List<ProductLevel3Image> convertMasterDataItemImagesToProductLevel3Images(
      Collection<MasterDataItemImageDTO> itemImages);

  ItemSummaryRequest convertProductLevel3ItemSearchToItemSummaryRequest(ProductLevel3ItemSearch search);

  ProductLevel3Item convertItemSummaryResponseToProductLevel3Item(ItemSummaryResponse source);

  List<ProductLevel3Logistics> convertLogisticDetailsToItemLevel3Logistics(
      List<GetSkuLogisticProductResponse> getSkuLogisticProductResponses);

  ItemsSummaryDetailRequest convertProductLevel3SummaryDetailsRequestToItemSummaryRequest(
      ProductLevel3SummaryFilterDetails filterRequest);

  ProductLevel3SummaryDetailsImage convertMasterDataItemImagesToProductLevel3SummaryDetailsImage(
      MasterDataItemImageDTO masterDataItemImages);

  CampaignPriceRequest convertItemSkusToCampaignPriceRequest(List<String> itemSkus, String categoryCode,
      Map<String, String> itemSkuAndPickupCodeMap, Map<String, Double> l5AndOfferPriceMap);

  Map<String, CampaignPriceSkuResponse> convertCampaignPriceResponseListToCampaignPriceSkuResponseMap(
      List<CampaignPriceResponse> campaignPriceResponseList);

  ProductLevel3SummaryDetailsImage convertMasterDataProductImagesToProductLevel3SummaryDetailsImage(
      ProductImage masterDataItemImages);
}
