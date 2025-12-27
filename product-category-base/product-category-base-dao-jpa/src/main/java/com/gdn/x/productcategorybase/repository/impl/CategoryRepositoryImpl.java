package com.gdn.x.productcategorybase.repository.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import com.gdn.x.productcategorybase.CategoryState;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DocumentType;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.repository.CategoryRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

/**
 * Created by Kesha on 09/06/16.
 */
public class CategoryRepositoryImpl implements CategoryRepositoryCustom {

  private static final Logger LOGGER = LoggerFactory.getLogger(CategoryRepositoryImpl.class);

  private static final String STORE_ID_PARAM = "storeId";
  private static final String CATALOG_ID_PARAM = "catalogId";
  private static final String DISPLAY_PARAM = "display";
  private static final String STORE_ID = "storeId";
  private static final String CATEGORY_NAME = "name";
  private static final String CATEGORY_NAME_ENGLISH = "nameEnglish";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String ACTIVATED = "activated";
  private static final String DOCUMENT_TYPE = "documentType";

  private static final String GET_FINAL_CATEGORY_SQL =
      "WITH RECURSIVE category_parent(id, "
          + " parent_category_id) AS ("
          + " SELECT id,parent_category_id FROM pcc_category WHERE "
          + " id = :category_id and mark_for_delete='f'"
          + " UNION ALL"
          + " SELECT c.id ,c.parent_category_id FROM category_parent AS p, pcc_category AS c WHERE c.id"
          + " = p.parent_category_id and c.mark_for_delete='f'" + " )"
          + " SELECT id FROM category_parent where parent_category_id is null;";

  private static final String GET_PARENT_CATEGORY_HIERARCHY_SQL =
      "WITH RECURSIVE category_parent(category_code, parent_category_id) AS ("
          + " SELECT category_code ,parent_category_id, id FROM pcc_category WHERE id = :category_id"
          + " and mark_for_delete=false UNION ALL SELECT c.category_code ,c.parent_category_id, c.id FROM"
          + " category_parent AS p, pcc_category AS c WHERE c.id = p.parent_category_id and c.mark_for_delete=false)"
          + " SELECT parent_category_id FROM category_parent where parent_category_id is not null;";

  private static final String GET_FINAL_CATEGORY_BY_CODE_SQL =
      "WITH RECURSIVE category_parent(category_code, "
          + " parent_category_id) AS ("
          + " SELECT category_code,parent_category_id FROM pcc_category WHERE "
          + " id = :category_id and mark_for_delete='f'"
          + " UNION ALL"
          + " SELECT c.category_code ,c.parent_category_id FROM category_parent AS p, pcc_category AS c WHERE c.id"
          + " = p.parent_category_id and c.mark_for_delete='f'" + " )"
          + " SELECT category_code FROM category_parent where parent_category_id is null;";

  private static final String CATEGORY_SUMMARY_BY_STORE_ID_AND_CATALOG_ID_SQL =
      "SELECT ctg.id, ctg.parent_category_id, ctg.category_code, ctg.name, ctg.display, ctg.store_id, ctg.created_date, ctg.created_by, ctg.updated_date, ctg.updated_by, ctg.mark_for_delete, ctg.sequence "
          + "FROM pcc_category ctg JOIN pcc_catalog cat ON ctg.catalog_id = cat.id "
          + "WHERE cat.mark_for_delete = false AND ctg.mark_for_delete = false AND ctg.store_id = :"
          + STORE_ID_PARAM + " AND ctg.catalog_id = :" + CATALOG_ID_PARAM;

  private static final String ROW_COUNT_BY_STORE_ID_AND_CATALOG_ID_SQL =
      "SELECT COUNT(ctg.id) FROM pcc_category ctg JOIN pcc_catalog cat ON ctg.catalog_id = cat.id "
          + "WHERE cat.mark_for_delete = false AND ctg.mark_for_delete = false AND ctg.store_id = :"
          + STORE_ID_PARAM + " AND ctg.catalog_id = :" + CATALOG_ID_PARAM;

  private static final String CATEGORY_SUMMARY_BY_STORE_ID_AND_CATALOG_ID_AND_DISPLAY_SQL =
      "SELECT ctg.id, ctg.parent_category_id, ctg.category_code, ctg.name, ctg.display, ctg.store_id, ctg.created_date, ctg.created_by, ctg.updated_date, ctg.updated_by, ctg.mark_for_delete, ctg.sequence "
          + "FROM pcc_category ctg JOIN pcc_catalog cat ON ctg.catalog_id = cat.id "
          + "WHERE cat.mark_for_delete = false AND ctg.mark_for_delete = false AND ctg.store_id = :"
          + STORE_ID_PARAM
          + " AND ctg.catalog_id = :" + CATALOG_ID_PARAM + " AND ctg.display = :" + DISPLAY_PARAM;

  private static final String ROW_COUNT_BY_STORE_ID_AND_CATALOG_ID_AND_DISPLAY_SQL =
      "SELECT COUNT(ctg.id) FROM pcc_category ctg JOIN pcc_catalog cat ON ctg.catalog_id = cat.id "
      + "WHERE cat.mark_for_delete = false AND ctg.mark_for_delete = false AND ctg.store_id = :"
          + STORE_ID_PARAM
          + " AND ctg.catalog_id = :"
          + CATALOG_ID_PARAM
          + " AND ctg.display = :"
          + DISPLAY_PARAM;

  @Autowired
  private NamedParameterJdbcTemplate namedJdbcTemplate;

  @PersistenceContext
  private EntityManager entityManager;


  @Override
  public String getFinalParentCategoryId(String categoryId) {
    Map<String, String> parameterMap = new HashMap<>();
    parameterMap.put("category_id", categoryId);
    LOGGER.debug("Getting final parent category for category {}", categoryId);
    return this.namedJdbcTemplate
        .queryForObject(GET_FINAL_CATEGORY_SQL, parameterMap, String.class);
  }

  @Override
  public List<String> getParentCategoryHierarchyByCategoryId(String categoryId) {
    Map<String, String> parameterMap = new HashMap<>();
    parameterMap.put("category_id", categoryId);
    LOGGER.debug("Getting parent category hierarchy for category {}", categoryId);
    return this.namedJdbcTemplate.queryForList(GET_PARENT_CATEGORY_HIERARCHY_SQL, parameterMap, String.class);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Page<Category> findCategorySummaryByStoreIdAndCatalogIdAndDisplay(String storeId,
      String catalogId, Boolean display, Pageable pageable) {
    List<Category> categories = new ArrayList<>();
    List<Object[]> queryResult;
    long totalRecords;
    if (display != null) {
      queryResult =
          this.entityManager
              .createNativeQuery(CATEGORY_SUMMARY_BY_STORE_ID_AND_CATALOG_ID_AND_DISPLAY_SQL)
              .setParameter(STORE_ID_PARAM, storeId).setParameter(CATALOG_ID_PARAM, catalogId)
              .setParameter(DISPLAY_PARAM, display).setFirstResult(pageable.getPageNumber())
              .setMaxResults(pageable.getPageSize()).getResultList();
      totalRecords =
          (Long) this.entityManager
              .createNativeQuery(ROW_COUNT_BY_STORE_ID_AND_CATALOG_ID_AND_DISPLAY_SQL)
              .setParameter(STORE_ID_PARAM, storeId).setParameter(CATALOG_ID_PARAM, catalogId)
              .setParameter(DISPLAY_PARAM, display).getSingleResult();
    } else {
      queryResult =
          this.entityManager.createNativeQuery(CATEGORY_SUMMARY_BY_STORE_ID_AND_CATALOG_ID_SQL)
              .setParameter(STORE_ID_PARAM, storeId).setParameter(CATALOG_ID_PARAM, catalogId)
              .setFirstResult(pageable.getPageNumber()).setMaxResults(pageable.getPageSize())
              .getResultList();
      totalRecords =
          (Long) this.entityManager
              .createNativeQuery(ROW_COUNT_BY_STORE_ID_AND_CATALOG_ID_SQL)
              .setParameter(STORE_ID_PARAM, storeId).setParameter(CATALOG_ID_PARAM, catalogId)
              .getSingleResult();
    }

    for (Object[] record : queryResult) {
      Category category = new Category();
      Category parentCategory = new Category();
      category.setId((String) record[0]);

      parentCategory.setId((String) record[1]);
      category.setParentCategory(parentCategory);

      category.setCategoryCode((String) record[2]);
      category.setName((String) record[3]);
      category.setDisplay((Boolean) record[4]);
      category.setStoreId((String) record[5]);
      category.setCreatedDate((Date) record[6]);
      category.setCreatedBy((String) record[7]);
      category.setUpdatedDate((Date) record[8]);
      category.setUpdatedBy((String) record[9]);
      category.setMarkForDelete((Boolean) record[10]);
      category.setSequence((Integer) record[11]);

      categories.add(category);
    }
    return new PageImpl<>(categories, pageable, totalRecords);
  }

  @Override
  public Page<Category> findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(String storeId, String name,
      String state, String documentFilterType, Pageable pageable) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Category> criteriaQuery = criteriaBuilder.createQuery(Category.class);
    Root<Category> categoryRoot = criteriaQuery.from(Category.class);
    Predicate predicate =
        getPredicateForCategoryFilter(storeId, name, state, documentFilterType, criteriaBuilder, categoryRoot);

    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<Category> countRoot = countCriteriaQuery.from(Category.class);
    Predicate countPredicate =
        getPredicateForCategoryFilter(storeId, name, state, documentFilterType, criteriaBuilder, countRoot);

    CriteriaQuery<Category> resultCriteriaQuery = criteriaQuery.select(categoryRoot).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);

    List<Category> categoryList =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(categoryList, pageable, totalRecords);
  }

  private Predicate getPredicateForCategoryFilter(String storeId, String name, String state, String documentFilterType,
      CriteriaBuilder criteriaBuilder, Root<Category> categoryRoot) {
    Predicate predicate = criteriaBuilder.equal(categoryRoot.get(STORE_ID), storeId);
    if (StringUtils.isNotEmpty(name)) {
      Predicate predicateForNameBahasha =
          criteriaBuilder.like(criteriaBuilder.lower(categoryRoot.get(CATEGORY_NAME)), name.toLowerCase());
      Predicate predicateForNameEnglish =
          criteriaBuilder.like(criteriaBuilder.lower(categoryRoot.get(CATEGORY_NAME_ENGLISH)), name.toLowerCase());
      Predicate predicateForName = criteriaBuilder.or(predicateForNameBahasha, predicateForNameEnglish);
      predicate = criteriaBuilder.and(predicate, predicateForName);
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(MARK_FOR_DELETE), false));
    if (CategoryState.ACTIVE.name().equals(state)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(ACTIVATED), true));
    } else if (CategoryState.INACTIVE.name().equals(state)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(ACTIVATED), false));
    }

    if (DocumentType.DOCUMENT_REQUIRED.getValue().equals(documentFilterType)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNotNull(categoryRoot.get(DOCUMENT_TYPE)));
    } else if (DocumentType.DOCUMENT_NOT_REQUIRED.getValue().equals(documentFilterType)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNull(categoryRoot.get(DOCUMENT_TYPE)));
    }
    return predicate;
  }

  @Override
  public Page<Category> findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
      String storeId, String parentCategoryId, String catalogId, String documentFilterType, boolean ignoreB2bExclusive,
      boolean filterHalalCategory, Pageable pageable) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Category> criteriaQuery = criteriaBuilder.createQuery(Category.class);
    Root<Category> categoryRoot = criteriaQuery.from(Category.class);
    Predicate predicate =
        getPredicateForCategoryFilterWithFlags(storeId, parentCategoryId, catalogId, documentFilterType,
            ignoreB2bExclusive, filterHalalCategory, criteriaBuilder, categoryRoot);

    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<Category> countRoot = countCriteriaQuery.from(Category.class);
    Predicate countPredicate =
        getPredicateForCategoryFilterWithFlags(storeId, parentCategoryId, catalogId, documentFilterType,
            ignoreB2bExclusive, filterHalalCategory, criteriaBuilder, countRoot);

    CriteriaQuery<Category> resultCriteriaQuery = criteriaQuery.select(categoryRoot).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);

    List<Category> categoryList =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(categoryList, pageable, totalRecords);
  }

  private Predicate getPredicateForCategoryFilterWithFlags(String storeId, String parentCategoryId, String catalogId,
      String documentFilterType, boolean ignoreB2bExclusive, boolean filterHalalCategory,
      CriteriaBuilder criteriaBuilder, Root<Category> categoryRoot) {
    Predicate predicate = criteriaBuilder.equal(categoryRoot.get(STORE_ID), storeId);
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(MARK_FOR_DELETE), false));
    if (StringUtils.isNotEmpty(parentCategoryId)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.equal(categoryRoot.get(Constants.PARENT_CATEGORY_ID), parentCategoryId));
    } else {
      predicate =
          criteriaBuilder.and(predicate, criteriaBuilder.isNull(categoryRoot.get(Constants.PARENT_CATEGORY_ID)));
    }
    if (StringUtils.isNotEmpty(catalogId)) {
      predicate =
          criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(Constants.CATALOG_ID), catalogId));
    }
    if (DocumentType.DOCUMENT_REQUIRED.getValue().equals(documentFilterType)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNotNull(categoryRoot.get(DOCUMENT_TYPE)));
    } else if (DocumentType.DOCUMENT_NOT_REQUIRED.getValue().equals(documentFilterType)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNull(categoryRoot.get(DOCUMENT_TYPE)));
    }
    if (ignoreB2bExclusive) {
      predicate =
          criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(Constants.B2B_EXCLUSIVE), false));
    }
    if (filterHalalCategory) {
      predicate =
          criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryRoot.get(Constants.HALAL_CATEGORY), true));
    }
    return predicate;
  }
}
