package com.gdn.mta.product.repository;

import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.ListJoin;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartner_;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.DatabaseFieldNames;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.model.vo.ProductLv1IdxLv3IdVO;

@Repository
public class ProductBusinessPartnerCustomRepositoryBean implements ProductBusinessPartnerCustomRepository {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public List<ProductLv1IdxLv3IdVO> findProductLv1IdxLv3IdVO(String businessPartnerId, String merchantSKu,
      List<String> productIds) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductLv1IdxLv3IdVO> query = criteriaBuilder.createQuery(ProductLv1IdxLv3IdVO.class);
    Root<ProductBusinessPartner> root = query.from(ProductBusinessPartner.class);
    Predicate predicate = criteriaBuilder.equal(root.get(DatabaseFieldNames.STORE_ID), 
        GdnMandatoryRequestParameterUtil.getStoreId());
    predicate = createEqualBusinessPartnerIdPredicate(businessPartnerId, predicate,
        criteriaBuilder, root);
    predicate = createEqualItemMerchantSkuPredicate(merchantSKu, predicate, criteriaBuilder, root);
    predicate = createInProductIdsPredicate(productIds, predicate, criteriaBuilder, root);
    query = query
        .multiselect(root.get(ProductBusinessPartner_.productId), root.get(ProductBusinessPartner_.gdnProductSku),
            root.get(ProductBusinessPartner_.preOrder)).where(predicate);
    return entityManager.createQuery(query)
        .getResultList();
  }
  
  private Predicate createEqualBusinessPartnerIdPredicate(String businessPartnerId, 
      Predicate predicate, CriteriaBuilder criteriaBuilder, Root<ProductBusinessPartner> root) {
    if (StringUtils.isNotBlank(businessPartnerId)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(
          root.get(DatabaseFieldNames.BUSINESS_PARTNER_ID), businessPartnerId));
    }
    return predicate;
  }
  
  private Predicate createEqualItemMerchantSkuPredicate(String merchantSKu,
      Predicate predicate, CriteriaBuilder criteriaBuilder, Root<ProductBusinessPartner> root) {
    if (StringUtils.isNotBlank(merchantSKu)) {
      ListJoin<ProductBusinessPartner, ProductItemBusinessPartner> join =
          root.join(ProductBusinessPartner_.productItemBusinessPartners);
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(
          join.get(DatabaseFieldNames.MERCHANT_SKU), merchantSKu));
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(
          join.get(DatabaseFieldNames.STORE_ID), GdnMandatoryRequestParameterUtil.getStoreId()));
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(
          join.get(DatabaseFieldNames.MARK_FOR_DELETE), false));
    }
    return predicate;
  }
  
  private Predicate createInProductIdsPredicate(List<String> productIds,
      Predicate predicate, CriteriaBuilder criteriaBuilder, Root<ProductBusinessPartner> root) {
    if (!CollectionUtils.isEmpty(productIds)) {
      CriteriaBuilder.In<Object> in = criteriaBuilder.in(root.get(DatabaseFieldNames.PRODUCT_ID));
      in.value(productIds);
      predicate = criteriaBuilder.and(predicate, in);
    }
    return predicate;
  }

  @Override
  public Page<ProductBusinessPartner> findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(String storeId,
      String businessPartnerId, String productName, Pageable pageable, String orderBy, String sortBy) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductBusinessPartner> query = builder.createQuery(ProductBusinessPartner.class);
    CriteriaQuery<Long> countCriteriaQuery = builder.createQuery(Long.class);
    Root<ProductBusinessPartner> businessPartnerRoot = query.from(ProductBusinessPartner.class);
    Root<ProductBusinessPartner> countRoot = countCriteriaQuery.from(ProductBusinessPartner.class);
    Predicate predicate = getPredicate(storeId, businessPartnerId, productName, builder, businessPartnerRoot);
    Predicate countQueryPredicate = getPredicate(storeId, businessPartnerId, productName, builder, countRoot);
    CriteriaQuery<ProductBusinessPartner> resultCriteriaQuery = query
        .multiselect(businessPartnerRoot.get(ProductBusinessPartner_.productId),
            businessPartnerRoot.get(ProductBusinessPartner_.productName),
            businessPartnerRoot.get(ProductBusinessPartner_.categoryName),
            businessPartnerRoot.get(ProductBusinessPartner_.brand),
            businessPartnerRoot.get(ProductBusinessPartner_.createdDate),
            businessPartnerRoot.get(ProductBusinessPartner_.createdBy),
            businessPartnerRoot.get(ProductBusinessPartner_.updatedDate)).where(predicate).distinct(true);
    countCriteriaQuery.select(builder.count(countRoot)).where(countQueryPredicate);
    if (SolrConstants.ASC.equalsIgnoreCase(sortBy)) {
      resultCriteriaQuery.orderBy(builder.asc(businessPartnerRoot.get(orderBy)));
    } else {
      resultCriteriaQuery.orderBy(builder.desc(businessPartnerRoot.get(orderBy)));
    }
    List<ProductBusinessPartner> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = (Long) entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicate(String storeId, String businessPartnerId, String productName, CriteriaBuilder builder,
      Root<ProductBusinessPartner> businessPartnerRoot) {
    Predicate predicate = builder.equal(businessPartnerRoot.get(DatabaseFieldNames.STORE_ID), storeId);
    predicate = builder.and(predicate,
        builder.equal(businessPartnerRoot.get(DatabaseFieldNames.BUSINESS_PARTNER_ID), businessPartnerId));
    predicate = builder
        .and(predicate, builder.equal(businessPartnerRoot.get(DatabaseFieldNames.STATE), Constants.DELETED_STATE));
    predicate =
        builder.and(predicate, builder.equal(businessPartnerRoot.get(DatabaseFieldNames.MARK_FOR_DELETE), true));
    if (StringUtils.isNotEmpty(productName)) {
      predicate = builder.and(predicate, builder
          .like(builder.lower(businessPartnerRoot.get(DatabaseFieldNames.PRODUCT_NAME)),
              DatabaseFieldNames.ANY_STRING + productName.toLowerCase().trim() + DatabaseFieldNames.ANY_STRING));
    }
    return predicate;
  }
}
