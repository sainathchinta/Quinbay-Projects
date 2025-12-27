package com.gdn.x.productcategorybase.service.categorytree;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.entity.categorytree.CategoryNode;
import com.gdn.x.productcategorybase.repository.categorytree.CategoryTreeRepository;

@Service
@Transactional(readOnly = true)
public class CategoryTreeServiceBean implements CategoryTreeService {

  @Autowired
  private CategoryTreeRepository categoryTreeRepository;

  @Override
  public List<CategoryNode> findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(String catalogCode,
      List<String> categoryCodes, boolean active) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    List<Object[]> categoryNodeRaws =
        this.categoryTreeRepository
            .findCategoryNodeByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(storeId,
                catalogCode, categoryCodes, active);
    return this.generateCategoryNodes(categoryNodeRaws);
  }

  @Override
  public List<CategoryNode> findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(String catalogCode,
      String parentCategoryCode, boolean active) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    List<Object[]> categoryNodeRaws;
    if (StringUtils.isEmpty(parentCategoryCode)) {
      categoryNodeRaws =
          this.categoryTreeRepository.findCategoryRootNodeByStoreIdAndCatalogCodeAndActiveAndMarkForDeleteFalse(
              storeId, catalogCode, active);
    } else {
      categoryNodeRaws =
          this.categoryTreeRepository
              .findCategoryNodeByStoreIdAndCatalogCodeAndParentCategoryCodeAndActiveAndMarkForDeleteFalse(storeId,
                  catalogCode, parentCategoryCode, active);
    }
    return this.generateCategoryNodes(categoryNodeRaws);
  }

  @Override
  public List<CategoryNode> findByCatalogCodeAndCategoryCodesAndActive(String catalogCode, List<String> categoryCodes,
      boolean active) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    List<Object[]> categoryNodeRaws =
        this.categoryTreeRepository.findByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(storeId,
            catalogCode, categoryCodes, active);
    return this.generateCategoryNodes(categoryNodeRaws);
  }

  private CategoryNode generateCategoryNode(Object[] categoryNodeRaw) throws Exception {
    CategoryNode categoryNode = new CategoryNode();
    categoryNode.setCategoryCode((String) categoryNodeRaw[0]);
    categoryNode.setCategoryName((String) categoryNodeRaw[1]);
    categoryNode.setParentCategoryCode((String) categoryNodeRaw[2]);
    categoryNode.setActive(Boolean.valueOf(String.valueOf(categoryNodeRaw[3])));
    categoryNode.setChildCount(Long.valueOf(String.valueOf(categoryNodeRaw[4])));
    return categoryNode;
  }

  private List<CategoryNode> generateCategoryNodes(List<Object[]> categoryNodeRaws) throws Exception {
    List<CategoryNode> categoryNodes = new ArrayList<CategoryNode>();
    for (Object[] categoryNodeRaw : categoryNodeRaws) {
      categoryNodes.add(this.generateCategoryNode(categoryNodeRaw));
    }
    return categoryNodes;
  }

}
