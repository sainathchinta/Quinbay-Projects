package com.gdn.mta.product.service;

import java.util.List;
import java.util.Set;


import org.apache.commons.lang3.tuple.Pair;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.L5HistoryDTO;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.x.product.model.vo.BundleRecipeVo;

public interface BundleRecipeService {

  /**
   * update bundle recipe in product item business partner
   * @param storeId
   * @param existingProductItemBusinessPartner
   * @param productBundleRecipe
   * @return
   * @throws JsonProcessingException
   */
  Pair<List<ProductItemBusinessPartner>, List<L5HistoryDTO>> updateBundleRecipeInProductItemBusinessPartner(
      String storeId, List<ProductItemBusinessPartner> existingProductItemBusinessPartner,
      List<ProductBundleRecipeRequest> productBundleRecipe) throws JsonProcessingException,
      ApplicationException;

  /**
   * convert bundle recipe to string
   * @param bundleRecipes
   * @return
   * @throws JsonProcessingException
   */
  String convertBundleRecipeToString(Set<BundleRecipeVo> bundleRecipes) throws JsonProcessingException;

}
