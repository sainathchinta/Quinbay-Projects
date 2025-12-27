package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.L5HistoryDTO;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BundleRecipeServiceImpl implements BundleRecipeService {

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Override
  public Pair<List<ProductItemBusinessPartner>, List<L5HistoryDTO>> updateBundleRecipeInProductItemBusinessPartner(
      String storeId, List<ProductItemBusinessPartner> existingProductItemBusinessPartner,
      List<ProductBundleRecipeRequest> productBundleRecipe)
      throws JsonProcessingException, ApplicationException {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList = new ArrayList<>();
    List<L5HistoryDTO> bundleHistory = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productBundleRecipe)) {
      Set<String> itemSkus = productBundleRecipe.stream()
          .map(ProductBundleRecipeRequest::getItemSku).collect(Collectors.toSet());
      productItemBusinessPartnerList = productItemBusinessPartnerService
          .getProductItemBusinessPartnerByItemSkuList(storeId, new ArrayList<>(itemSkus));
      productItemBusinessPartnerList = productItemBusinessPartnerService
          .replaceNewWithExistingProductItemBusinessPartner(existingProductItemBusinessPartner,
              productItemBusinessPartnerList);
      Map<String, Map<String, String>> bundleHistoryMap =
          updateBundlingRecipeInProductItemBusinessPartner(productItemBusinessPartnerList,
              productBundleRecipe);
      bundleHistory = convertBundleHistoryToL5HistoryDtoList(bundleHistoryMap);
    }
    return Pair.of(productItemBusinessPartnerList, bundleHistory);
  }

  public List<L5HistoryDTO> convertBundleHistoryToL5HistoryDtoList(
      Map<String, Map<String, String>> bundleHistoryMap) {
    return bundleHistoryMap.entrySet().stream().map(entry -> new L5HistoryDTO(entry.getKey(),
            Constants.HYPHEN, Arrays.asList(entry.getValue()))).collect(Collectors.toList());
  }

  public Map<String, Map<String, String>> updateBundlingRecipeInProductItemBusinessPartner(
      List<ProductItemBusinessPartner> productItemBusinessPartnerList,
      List<ProductBundleRecipeRequest> productBundleRecipe)
      throws JsonProcessingException, ApplicationException {
    Map<String, Map<String, String>> bundleHistoryMap = new HashMap<> ();
    Map<String, Set<ProductBundleRecipe>> itemSkuAndBundlingRecipeMap =
        convertToItemSkuAndBundlingRecipeMap(productBundleRecipe);
    Map<String,String> itemSkuAndItemNameMap = itemNameAndItemSkuMap(itemSkuAndBundlingRecipeMap.keySet());
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList) {
      String itemSku = productItemBusinessPartner.getGdnProductItemSku();
      if (!bundleHistoryMap.containsKey(itemSku)) {
        bundleRecipeHistoryInNeedRevision(itemSku, bundleHistoryMap,
            itemSkuAndBundlingRecipeMap.get(itemSku), convertStringToBundleRecipeRequestList(
                productItemBusinessPartner.getBundleRecipe()), itemSkuAndItemNameMap);
      }
      productItemBusinessPartner.setBundleRecipe(objectMapper.writeValueAsString(
          itemSkuAndBundlingRecipeMap.get(productItemBusinessPartner.getGdnProductItemSku())));
    }
    return bundleHistoryMap;
  }

  public Map<String, Set<ProductBundleRecipe>> convertToItemSkuAndBundlingRecipeMap(
      List<ProductBundleRecipeRequest> productBundleRecipe) {
    Map<String, Set<ProductBundleRecipe>> itemSkuAndBundlingRecipeMap = new HashMap<>();
    for (ProductBundleRecipeRequest productBundleRecipeRequest : productBundleRecipe) {
      itemSkuAndBundlingRecipeMap.put(productBundleRecipeRequest.getItemSku(),
          productBundleRecipeRequest.getBundleRecipe());
    }
    return itemSkuAndBundlingRecipeMap;
  }

  public void bundleRecipeHistoryInNeedRevision(String itemSku,
      Map<String, Map<String, String>> bundleHistoryMap,
      Set<ProductBundleRecipe> newBundleRecipeRequests,
      Set<ProductBundleRecipe> prevBundleRecipeRequests, Map<String, String> itemSkuAndNameMap) {
    String previousValue = convertProductBundleRecipeListToHistory(prevBundleRecipeRequests);
    String currentValue = convertProductBundleRecipeListToHistory(newBundleRecipeRequests);
    if (!StringUtils.equals(previousValue, currentValue)) {
      Map<String, String> historyMap = new HashMap<>();
      historyMap.put(Constants.PREVIOUS_VALUE, previousValue);
      historyMap.put(Constants.CURRENT_VALUE, currentValue);
      historyMap.put(Constants.HISTORY_ACTIVITY,
          UpdateProductActivity.BUNDLE_RECIPE_CHANGE.getDesc() + Constants.NEED_REVISION);
      historyMap.put(Constants.ITEM_NAME, itemSkuAndNameMap.getOrDefault(itemSku, StringUtils.EMPTY));
      bundleHistoryMap.put(itemSku, historyMap);
    }
  }

  public String convertProductBundleRecipeListToHistory(Set<ProductBundleRecipe> productBundleRecipes) {
    if (CollectionUtils.isNotEmpty(productBundleRecipes)) {
      StringBuilder history = new StringBuilder();
      for (ProductBundleRecipe recipe : productBundleRecipes) {
        history.append(recipe.getQuantity()).append('*').append(recipe.getItemSku()).append(" + ");
      }
      history.setLength(history.length() - 3);
      return history.toString();
    } else {
      return Constants.HYPHEN;
    }
  }

  public Set<ProductBundleRecipe> convertStringToBundleRecipeRequestList(String bundleRecipe)
      throws JsonProcessingException {
    if (StringUtils.isNotEmpty(bundleRecipe)) {
      return objectMapper.readValue(bundleRecipe, new TypeReference<Set<ProductBundleRecipe>>() {});
    } else {
      return new HashSet<>();
    }
  }

  public Map<String, String> itemNameAndItemSkuMap(Set<String> itemSkus) throws ApplicationException {
    GdnRestSingleResponse<SimpleMapStringResponse> response =  xProductOutbound
        .getItemNameByItemSkus(new SimpleListStringRequest(new ArrayList<>(itemSkus),
        false), true);
    return response.getValue().getValue();
  }

  @Override
  public String convertBundleRecipeToString(Set<BundleRecipeVo> bundleRecipes) throws JsonProcessingException {
    Set<ProductBundleRecipe> productBundleRecipes = Optional.ofNullable(bundleRecipes)
        .orElse(new HashSet<>()).stream().map(bundleRecipe -> new ProductBundleRecipe(bundleRecipe.getItemSku(),
            bundleRecipe.getQuantity())).collect(Collectors.toSet());
    return objectMapper.writeValueAsString(productBundleRecipes);
  }

}
