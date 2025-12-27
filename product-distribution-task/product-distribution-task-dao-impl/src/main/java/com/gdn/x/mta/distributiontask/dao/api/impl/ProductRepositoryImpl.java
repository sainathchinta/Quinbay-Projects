package com.gdn.x.mta.distributiontask.dao.api.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Tuple;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.mta.distributiontask.dao.api.ProductCustomRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProductRepositoryImpl implements ProductCustomRepository {

  private static final String STORE_ID = "storeId";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String WORKFLOW_STATE = "state";
  private static final String PRODUCT_CREATED_DATE = "productCreatedDate";
  private static final String PRODUCT_PREDICTION_SCORE = "productPredictionScore";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String IMAGE_APPROVER_ASSIGNEE = "imageApproverAssignee";
  private static final String CONTENT_APPROVER_ASSIGNEE = "contentApproverAssignee";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CREATED_BY = "createdBy";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String ANY_STRING = "%";
  private static final String EXTERNAL = "external";
  private static final String INTERNAL = "internal";
  private static final String IMAGE_ASSIGNED_DATE = "imageAssignedDate";
  private static final String CONTENT_ASSIGNED_DATE = "contentAssignedDate";
  private static final String CURRENT_VENDOR = "currentVendor";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String POST_LIVE = "postLive";
  private static final String ORDER_BY_DESC = "desc";
  private static final String IMAGE_VIOLATIONS = "imageViolations";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
  private static final String BRAND_APPROVED_STATE = "APPROVED";
  private static final String GOOD_EN = "Good";
  private static final String BRAND_NOT_APPROVED_STATUS_FILTER = "brandNotApproved";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public List<ProductBusinessPartnerMapperResponse> findByStoreIdAndKeywordAndStateAndUpdatedDate(String storeId,
      String keyword, List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, Pageable pageable,
      String vendorCode, Boolean brandPending) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Tuple> criteriaQuery = criteriaBuilder.createTupleQuery();
    Root<Product> product = criteriaQuery.from(Product.class);
    Predicate predicate =
        getPredicateForRequestFilters(storeId, keyword.toLowerCase(), state, startDate, endDate, assignment, vendorCode,
            criteriaBuilder, product, brandPending);
    criteriaQuery.distinct(true).multiselect(product.get(BUSINESS_PARTNER_CODE), product.get(BUSINESS_PARTNER_NAME))
        .where(predicate);
    List<Tuple> tupleResult =
        entityManager.createQuery(criteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse;
    List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList = new ArrayList<>();
    for (Tuple t : tupleResult) {
      String business_partner_code = (String) t.get(0);
      String business_partner_name = (String) t.get(1);
      productBusinessPartnerMapperResponse =
          new ProductBusinessPartnerMapperResponse(business_partner_code, business_partner_name);
      productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    }
    return productBusinessPartnerMapperResponseList;
  }

  @Override
  public List<String> findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(String storeId,
      String keyword, List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      Boolean brandPending) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Tuple> criteriaQuery = criteriaBuilder.createTupleQuery();
    Root<Product> product = criteriaQuery.from(Product.class);
    Predicate predicate =
        getPredicateForAssigneeRequestFiltersContentAndImagePending(storeId, keyword.toLowerCase(), state, startDate,
            endDate, assignment, vendorCode, criteriaBuilder, product, brandPending);
    criteriaQuery.multiselect(product.get(IMAGE_APPROVER_ASSIGNEE), product.get(CONTENT_APPROVER_ASSIGNEE))
        .where(predicate);
    List<Tuple> tupleResult = entityManager.createQuery(criteriaQuery).getResultList();
    List<String> assigneeEmailIdList = new ArrayList<>();
    for (Tuple t : tupleResult) {
      String imageAssignee = (String) t.get(0);
      String contentAssignee = (String) t.get(1);
      if (StringUtils.isNotEmpty(imageAssignee)) {
        assigneeEmailIdList.add(imageAssignee);
      }
      if (StringUtils.isNotEmpty(contentAssignee)) {
        assigneeEmailIdList.add(contentAssignee);
      }
    }
    return assigneeEmailIdList;
  }

  @Override
  public List<String> findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentPending(String storeId,
      String keyword, List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      Boolean brandPending) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Tuple> criteriaQuery = criteriaBuilder.createTupleQuery();
    Root<Product> product = criteriaQuery.from(Product.class);
    Predicate predicate =
        getPredicateForAssigneeRequestFiltersContentPending(storeId, keyword.toLowerCase(), state, startDate, endDate,
            assignment, vendorCode, criteriaBuilder, product, brandPending);
    criteriaQuery.multiselect(product.get(CONTENT_APPROVER_ASSIGNEE)).where(predicate);
    List<Tuple> tupleResult = entityManager.createQuery(criteriaQuery).getResultList();
    List<String> assigneeEmailIdList = new ArrayList<>();
    for (Tuple t : tupleResult) {
      assigneeEmailIdList.add((String) t.get(0));
    }
    return assigneeEmailIdList;
  }

  @Override
  public List<String> findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateImagePending(String storeId, String keyword,
      List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      Boolean brandPending) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Tuple> criteriaQuery = criteriaBuilder.createTupleQuery();
    Root<Product> product = criteriaQuery.from(Product.class);
    Predicate predicate =
        getPredicateForAssigneeRequestFiltersImagePending(storeId, keyword.toLowerCase(), state, startDate, endDate,
            assignment, vendorCode, criteriaBuilder, product, brandPending);
    criteriaQuery.multiselect(product.get(IMAGE_APPROVER_ASSIGNEE)).where(predicate);
    List<Tuple> tupleResult = entityManager.createQuery(criteriaQuery).getResultList();
    List<String> assigneeEmailIdList = new ArrayList<>();
    for (Tuple t : tupleResult) {
      assigneeEmailIdList.add((String) t.get(0));
    }
    return assigneeEmailIdList;
  }

  @Override
  public Page<Product> findProductByStoreIdAndKeywordAndStateAndUpdatedDateAndCategoryAndBusinessPartnerCodeAndAssigneeEmailId(
      String storeId, String keyword, List<WorkflowState> state, Date startDate, Date endDate,
      List<String> categoryCodes, String businessPartnerCode, String assigneeEmailId, String sortOrderByCreatedDate,
      Boolean assignment, Pageable pageable, String vendorCode, boolean postLive, String faultyImageType,
      Boolean brandPending) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Product> criteriaQuery = criteriaBuilder.createQuery(Product.class);
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<Product> product = criteriaQuery.from(Product.class);
    List<Order> orderArrayList = new ArrayList<>();
    Predicate predicate =
        getPredicateForSummaryFilter(storeId, keyword.toLowerCase(), state, startDate, endDate, categoryCodes,
            businessPartnerCode, assigneeEmailId, assignment, vendorCode, criteriaBuilder, product, postLive,
            faultyImageType, brandPending);
    CriteriaQuery<Product> resultCriteriaQuery = criteriaQuery.select(product).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countCriteriaQuery.from(Product.class))).where(predicate);
    if (ORDER_BY_DESC.equalsIgnoreCase(sortOrderByCreatedDate)) {
      orderArrayList.add(criteriaBuilder.asc(product.get(PRODUCT_PREDICTION_SCORE)));
      orderArrayList.add(criteriaBuilder.desc(product.get(PRODUCT_CREATED_DATE)));
    }
    else {
      orderArrayList.add(criteriaBuilder.desc(product.get(PRODUCT_PREDICTION_SCORE)));
      orderArrayList.add(criteriaBuilder.asc(product.get(PRODUCT_CREATED_DATE)));
    }
      resultCriteriaQuery.orderBy(orderArrayList);
    List<Product> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = (Long) entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicateForRequestFilters(String storeId, String keyword, List<WorkflowState> state,
      Date startDate, Date endDate, Boolean assignment, String vendorCode, CriteriaBuilder criteriaBuilder,
      Root<Product> product, Boolean brandPending) {
    Predicate predicate = criteriaBuilder.equal(product.get(STORE_ID), storeId);
    if (StringUtils.isNotEmpty(keyword) && !EXTERNAL.equals(keyword)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .like(criteriaBuilder.lower(product.get(BUSINESS_PARTNER_NAME)), ANY_STRING + keyword + ANY_STRING));
    } else if (StringUtils.isNotEmpty(keyword) && EXTERNAL.equals(keyword)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.notEqual(criteriaBuilder.lower(product.get(BUSINESS_PARTNER_NAME)), INTERNAL));
    }
    if (CollectionUtils.isNotEmpty(state)) {
      predicate = criteriaBuilder.and(predicate, (product.get(WORKFLOW_STATE).in(state)));
    }
    if (Objects.nonNull(assignment)) {
      if (Boolean.FALSE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .or(criteriaBuilder.isNull(product.get(IMAGE_ASSIGNED_DATE)),
                criteriaBuilder.isNull(product.get(CONTENT_ASSIGNED_DATE))));
      } else if (Boolean.TRUE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .and(criteriaBuilder.isNotNull(product.get((IMAGE_ASSIGNED_DATE))),
                criteriaBuilder.isNotNull(product.get(CONTENT_ASSIGNED_DATE))));

      }
    }
    if(Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.between(product.get(PRODUCT_CREATED_DATE), startDate, endDate));
    } else if (Objects.nonNull(endDate) && Objects.isNull(startDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.lessThanOrEqualTo(product.get(PRODUCT_CREATED_DATE), endDate));
    }
    if (Objects.nonNull(brandPending) && Boolean.TRUE.equals(brandPending)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.notEqual(criteriaBuilder.upper(product.get(BRAND_APPROVAL_STATUS)), BRAND_APPROVED_STATE));
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(product.get(MARK_FOR_DELETE), false));
    CriteriaBuilder vendorCriteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Vendor> vendorQuery = vendorCriteriaBuilder.createQuery(Vendor.class);
    Root<Vendor> vendor = vendorQuery.from(Vendor.class);
    vendorQuery.where(
        vendorCriteriaBuilder.and(vendorCriteriaBuilder.equal(vendor.get(VENDOR_CODE), vendorCode),
            vendorCriteriaBuilder.equal(vendor.get(MARK_FOR_DELETE), false)));
    TypedQuery<Vendor> vendorTypedQuery = entityManager.createQuery(vendorQuery);
    predicate = criteriaBuilder
        .and(predicate, criteriaBuilder.equal(product.get(CURRENT_VENDOR), vendorTypedQuery.getSingleResult()));
    return predicate;
  }

  private Predicate getPredicateForAssigneeRequestFiltersContentAndImagePending(String storeId, String keyword,
      List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      CriteriaBuilder criteriaBuilder, Root<Product> product, Boolean brandPending) {
    Predicate predicate = criteriaBuilder.equal(product.get(STORE_ID), storeId);
    if (StringUtils.isNotEmpty(keyword)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.or((criteriaBuilder
              .like(criteriaBuilder.lower(product.get(IMAGE_APPROVER_ASSIGNEE)), ANY_STRING + keyword + ANY_STRING)),
          (criteriaBuilder.like(criteriaBuilder.lower(product.get(CONTENT_APPROVER_ASSIGNEE)),
              ANY_STRING + keyword + ANY_STRING))));
    }
    if (CollectionUtils.isNotEmpty(state)) {
      predicate = criteriaBuilder.and(predicate, (product.get(WORKFLOW_STATE).in(state)));
    }
    if (Objects.nonNull(assignment)) {
      if (Boolean.TRUE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .and(criteriaBuilder.isNotNull(product.get((IMAGE_ASSIGNED_DATE))),
                criteriaBuilder.isNotNull(product.get(CONTENT_ASSIGNED_DATE))));

      }
    }
    if (Objects.nonNull(brandPending) && Boolean.TRUE.equals(brandPending)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.notEqual(criteriaBuilder.upper(product.get(BRAND_APPROVAL_STATUS)), BRAND_APPROVED_STATE));
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder
        .or(criteriaBuilder.isNotNull(product.get(CONTENT_APPROVER_ASSIGNEE)),
            criteriaBuilder.isNotNull(product.get(IMAGE_APPROVER_ASSIGNEE))));
    if(Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.between(product.get(PRODUCT_CREATED_DATE), startDate, endDate));
    } else if (Objects.nonNull(endDate) && Objects.isNull(startDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.lessThanOrEqualTo(product.get(PRODUCT_CREATED_DATE), endDate));
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(product.get(MARK_FOR_DELETE), false));
    CriteriaBuilder vendorCriteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Vendor> vendorQuery = vendorCriteriaBuilder.createQuery(Vendor.class);
    Root<Vendor> vendor = vendorQuery.from(Vendor.class);
    vendorQuery.where(
        vendorCriteriaBuilder.and(vendorCriteriaBuilder.equal(vendor.get(VENDOR_CODE), vendorCode),
            vendorCriteriaBuilder.equal(vendor.get(MARK_FOR_DELETE), false)));
    TypedQuery<Vendor> vendorTypedQuery = entityManager.createQuery(vendorQuery);
    predicate = criteriaBuilder
        .and(predicate, criteriaBuilder.equal(product.get(CURRENT_VENDOR), vendorTypedQuery.getSingleResult()));
    return predicate;
  }

  private Predicate getPredicateForAssigneeRequestFiltersContentPending(String storeId, String keyword,
      List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      CriteriaBuilder criteriaBuilder, Root<Product> product, Boolean brandPending) {
    Predicate predicate = criteriaBuilder.equal(product.get(STORE_ID), storeId);
    if (StringUtils.isNotEmpty(keyword)) {
      predicate = criteriaBuilder.and(predicate, (criteriaBuilder
          .like(criteriaBuilder.lower(product.get(CONTENT_APPROVER_ASSIGNEE)), ANY_STRING + keyword + ANY_STRING)));
    }
    if (CollectionUtils.isNotEmpty(state)) {
      predicate = criteriaBuilder.and(predicate, (product.get(WORKFLOW_STATE).in(state)));
    }
    if (Objects.nonNull(assignment)) {
      if (Boolean.TRUE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .and(criteriaBuilder.isNotNull(product.get((IMAGE_ASSIGNED_DATE))),
                criteriaBuilder.isNotNull(product.get(CONTENT_ASSIGNED_DATE))));

      }
    }
    if(Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.between(product.get(PRODUCT_CREATED_DATE), startDate, endDate));
    } else if (Objects.nonNull(endDate) && Objects.isNull(startDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.lessThanOrEqualTo(product.get(PRODUCT_CREATED_DATE), endDate));
    }
    if (Objects.nonNull(brandPending) && Boolean.TRUE.equals(brandPending)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.notEqual(criteriaBuilder.upper(product.get(BRAND_APPROVAL_STATUS)), BRAND_APPROVED_STATE));
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNotNull(product.get(CONTENT_APPROVER_ASSIGNEE)));
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(product.get(MARK_FOR_DELETE), false));
    CriteriaBuilder vendorCriteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Vendor> vendorQuery = vendorCriteriaBuilder.createQuery(Vendor.class);
    Root<Vendor> vendor = vendorQuery.from(Vendor.class);
    vendorQuery.where(
        vendorCriteriaBuilder.and(vendorCriteriaBuilder.equal(vendor.get(VENDOR_CODE), vendorCode),
            vendorCriteriaBuilder.equal(vendor.get(MARK_FOR_DELETE), false)));
    TypedQuery<Vendor> vendorTypedQuery = entityManager.createQuery(vendorQuery);
    predicate = criteriaBuilder
        .and(predicate, criteriaBuilder.equal(product.get(CURRENT_VENDOR), vendorTypedQuery.getSingleResult()));
    return predicate;
  }

  private Predicate getPredicateForAssigneeRequestFiltersImagePending(String storeId, String keyword,
      List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      CriteriaBuilder criteriaBuilder, Root<Product> product, Boolean brandPending) {
    Predicate predicate = criteriaBuilder.equal(product.get(STORE_ID), storeId);
    if (StringUtils.isNotEmpty(keyword)) {
      predicate = criteriaBuilder.and(predicate, (criteriaBuilder
          .like(criteriaBuilder.lower(product.get(IMAGE_APPROVER_ASSIGNEE)), ANY_STRING + keyword + ANY_STRING)));
    }
    if (CollectionUtils.isNotEmpty(state)) {
      predicate = criteriaBuilder.and(predicate, (product.get(WORKFLOW_STATE).in(state)));
    }
    if (Objects.nonNull(assignment)) {
      if (Boolean.TRUE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .and(criteriaBuilder.isNotNull(product.get((IMAGE_ASSIGNED_DATE))),
                criteriaBuilder.isNotNull(product.get(CONTENT_ASSIGNED_DATE))));

      }
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNotNull(product.get(IMAGE_APPROVER_ASSIGNEE)));
    if(Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.between(product.get(PRODUCT_CREATED_DATE), startDate, endDate));
    } else if (Objects.nonNull(endDate) && Objects.isNull(startDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.lessThanOrEqualTo(product.get(PRODUCT_CREATED_DATE), endDate));
    }
    if (Objects.nonNull(brandPending) && Boolean.TRUE.equals(brandPending)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.notEqual(criteriaBuilder.upper(product.get(BRAND_APPROVAL_STATUS)), BRAND_APPROVED_STATE));
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(product.get(MARK_FOR_DELETE), false));
    CriteriaBuilder vendorCriteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Vendor> vendorQuery = vendorCriteriaBuilder.createQuery(Vendor.class);
    Root<Vendor> vendor = vendorQuery.from(Vendor.class);
    vendorQuery.where(
        vendorCriteriaBuilder.and(vendorCriteriaBuilder.equal(vendor.get(VENDOR_CODE), vendorCode),
            vendorCriteriaBuilder.equal(vendor.get(MARK_FOR_DELETE), false)));
    TypedQuery<Vendor> vendorTypedQuery = entityManager.createQuery(vendorQuery);
    predicate = criteriaBuilder
        .and(predicate, criteriaBuilder.equal(product.get(CURRENT_VENDOR), vendorTypedQuery.getSingleResult()));
    return predicate;
  }

  private Predicate getPredicateForSummaryFilter(String storeId, String keyword, List<WorkflowState> state,
      Date startDate, Date endDate, List<String> categoryCodes, String businessPartnerCode, String assigneeEmailId,
      Boolean assignment, String vendorCode, CriteriaBuilder criteriaBuilder, Root<Product> product, boolean postLive,
      String faultyImageType, Boolean brandPending) {
    Predicate predicate = criteriaBuilder.equal((product.get(STORE_ID)), storeId);
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal((product.get(POST_LIVE)), postLive));
    if (StringUtils.isNotEmpty(keyword)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .or(criteriaBuilder.like(criteriaBuilder.lower(product.get(PRODUCT_NAME)), ANY_STRING + keyword + ANY_STRING),
              criteriaBuilder.equal(criteriaBuilder.lower(product.get(PRODUCT_CODE)), keyword),
              criteriaBuilder.like(criteriaBuilder.lower(product.get(CREATED_BY)), ANY_STRING + keyword + ANY_STRING)));
    }

    if (CollectionUtils.isNotEmpty(state)) {
      predicate = criteriaBuilder.and(predicate, (product.get(WORKFLOW_STATE).in(state)));
    }

    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      predicate = criteriaBuilder.and(predicate, (product.get(CATEGORY_CODE).in(categoryCodes)));
    }
    if (Objects.nonNull(assignment)) {
      if (Boolean.FALSE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .or(criteriaBuilder.isNull(product.get(IMAGE_ASSIGNED_DATE)),
                criteriaBuilder.isNull(product.get(CONTENT_ASSIGNED_DATE))));
      } else if (Boolean.TRUE.equals(assignment)) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .and(criteriaBuilder.isNotNull(product.get((IMAGE_ASSIGNED_DATE))),
                criteriaBuilder.isNotNull(product.get(CONTENT_ASSIGNED_DATE))));

      }
    }
    if(Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.between(product.get(PRODUCT_CREATED_DATE), startDate, endDate));
    } else if (Objects.nonNull(endDate) && Objects.isNull(startDate)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.lessThanOrEqualTo(product.get(PRODUCT_CREATED_DATE), endDate));
    }
    if (StringUtils.isNotEmpty(businessPartnerCode)) {
      if (!EXTERNAL.equalsIgnoreCase(businessPartnerCode)) {
        predicate = criteriaBuilder
            .and(predicate, criteriaBuilder.equal(product.get(BUSINESS_PARTNER_CODE), businessPartnerCode));
      } else {
        predicate = criteriaBuilder.and(predicate,
            criteriaBuilder.notEqual(criteriaBuilder.lower(product.get(BUSINESS_PARTNER_CODE)), INTERNAL));
      }
    }
    if (StringUtils.isNotEmpty(assigneeEmailId)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .or(criteriaBuilder.equal(product.get(IMAGE_APPROVER_ASSIGNEE), assigneeEmailId),
              criteriaBuilder.equal(product.get(CONTENT_APPROVER_ASSIGNEE), assigneeEmailId)));
    }
    if (Objects.nonNull(faultyImageType) && GOOD_EN.equalsIgnoreCase(faultyImageType)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.isNull(product.get(IMAGE_VIOLATIONS)));
    } else if (Objects.nonNull(faultyImageType)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.like(product.get(IMAGE_VIOLATIONS), ANY_STRING + faultyImageType + ANY_STRING));
    }
    if (Objects.nonNull(brandPending) && Boolean.TRUE.equals(brandPending)) {
      predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.notEqual(criteriaBuilder.upper(product.get(BRAND_APPROVAL_STATUS)), BRAND_APPROVED_STATE));
    }
    predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(product.get(MARK_FOR_DELETE), false));
    CriteriaBuilder vendorCriteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Vendor> vendorQuery = vendorCriteriaBuilder.createQuery(Vendor.class);
    Root<Vendor> vendor = vendorQuery.from(Vendor.class);
    vendorQuery.where(
        vendorCriteriaBuilder.and(vendorCriteriaBuilder.equal(vendor.get(VENDOR_CODE), vendorCode),
            vendorCriteriaBuilder.equal(vendor.get(MARK_FOR_DELETE), false)));
    TypedQuery<Vendor> vendorTypedQuery = entityManager.createQuery(vendorQuery);
    predicate = criteriaBuilder
        .and(predicate, criteriaBuilder.equal(product.get(CURRENT_VENDOR), vendorTypedQuery.getSingleResult()));
    return predicate;
  }
}
