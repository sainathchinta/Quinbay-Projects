package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.rest.web.model.response.IgnoreAttributeSet;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.service.api.CategoryService;
import com.gdn.x.product.service.api.ProductScoreRuleService;
import com.gdn.x.product.service.api.SystemParameterService;
import java.util.List;

@Service
public class CategoryServiceImpl implements CategoryService {

  private static final String CATEGORY_CODE_MUST_NOT_BE_BLANK = "categoryCode must not be blank";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String CATALOG_CODE_MUST_NOT_BE_BLANK = "catalogCode must not be blank";

  @Autowired
  @Lazy
  private ProductAndItemSolrRepository solrRepository;

  @Autowired
  private ProductScoreRuleService productScoreRuleService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public Page<ProductAndItemSolr> getProductsByMasterCatalog(String storeId, String catalogCode,
      String categoryCode, boolean searchEmptySalesOnly, Pageable page) {
    checkArgument(StringUtils.isNotBlank(storeId), CategoryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(catalogCode),
        CategoryServiceImpl.CATALOG_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(categoryCode),
        CategoryServiceImpl.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    return this.solrRepository.getProductsByMasterCatalog(storeId, catalogCode, categoryCode,
        searchEmptySalesOnly, page);
  }

  @Override
  public Page<ProductAndItemSolr> getProductsBySalesCatalog(String storeId, String catalogCode,
      String categoryCode, Pageable page) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CategoryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(catalogCode),
        CategoryServiceImpl.CATALOG_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(categoryCode),
        CategoryServiceImpl.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    return this.solrRepository.getProductsBySalesCatalog(storeId, catalogCode, categoryCode, page);
  }

  @Override
  public ProductScoreRuleResponse getProductScoreRuleForCategory(String categoryCode) throws Exception {
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setCategoryCode(categoryCode);
    productScoreRuleResponse.setIgnoreSymbols(objectMapper.readValue(systemParameterService
            .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_SYMBOLS_VARIABLE_NAME).getValue(),
        List.class));
    productScoreRuleResponse.setIgnoreAttributes(objectMapper.readValue(systemParameterService
            .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.IGNORE_ATTRIBUTE_VARIABLE_NAME).getValue(),
        new TypeReference<List<IgnoreAttributeSet>>() {
        }));

    if (StringUtils.isBlank(categoryCode)) {
      productScoreRuleResponse
          .setProductScoreRules(productScoreRuleService.getProductScoreRulesGlobal(Constants.DEFAULT_STORE_ID));
    } else {
      productScoreRuleResponse.setProductScoreRules(
          productScoreRuleService.getProductScoreRulesForCategory(Constants.DEFAULT_STORE_ID, categoryCode));
    }
    return productScoreRuleResponse;
  }
}
