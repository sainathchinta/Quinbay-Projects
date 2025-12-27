package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.Selection;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.param.PageableHelper;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.partners.pbp.commons.util.DatabaseFieldNames;

public class ProductBusinessPartnerCustomRepositoryBeanTest {

  @InjectMocks
  private ProductBusinessPartnerCustomRepositoryBean productBusinessPartnerCustomRepositoryBean;

  @Mock
  private EntityManager entityManager;

  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PRODUCT_NAME = "categoryName";
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "asc";
  private static final int PAGE = 1;
  private static final int SIZE = 10;
  private static final long TOTAL = 10;
  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private CriteriaBuilder criteriaBuilderMock;
  private CriteriaQuery<ProductBusinessPartner> criteriaQueryMock;
  private Root<ProductBusinessPartner> businessPartnerRoot;
  private CriteriaQuery<Long> countCriteriaQueryMock;
  private CriteriaQuery<ProductBusinessPartner> resultQuery;
  private TypedQuery<ProductBusinessPartner> productBusinessPartnerTypedQuery;
  private TypedQuery<Long> countTypedQuery;
  private Predicate predicate;
  private Expression<Long> longExpression;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    resultQuery = mock(CriteriaQuery.class);
    countCriteriaQueryMock = mock(CriteriaQuery.class);
    criteriaBuilderMock = mock(CriteriaBuilder.class);
    criteriaQueryMock = mock(CriteriaQuery.class);
    businessPartnerRoot = mock(Root.class);
    predicate = mock(Predicate.class);
    longExpression = mock(Expression.class);
    productBusinessPartnerTypedQuery = mock(TypedQuery.class);
    countTypedQuery = mock(TypedQuery.class);
    Path storeIdPath = mock(Path.class);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilderMock);
    when(criteriaBuilderMock.createQuery(ProductBusinessPartner.class)).thenReturn(criteriaQueryMock);
    when(criteriaBuilderMock.createQuery(Long.class)).thenReturn(countCriteriaQueryMock);
    when(entityManager.createQuery(resultQuery)).thenReturn(productBusinessPartnerTypedQuery);
    when(entityManager.createQuery(countCriteriaQueryMock)).thenReturn(countTypedQuery);
    when(criteriaQueryMock.from(ProductBusinessPartner.class)).thenReturn(businessPartnerRoot);
    when(countCriteriaQueryMock.from(ProductBusinessPartner.class)).thenReturn(businessPartnerRoot);
    when(businessPartnerRoot.get(DatabaseFieldNames.STORE_ID)).thenReturn(storeIdPath);
    when(criteriaQueryMock.multiselect(any(), any(), any(), any(), any(), any(), any())).thenReturn(resultQuery);
    when(resultQuery.where((Predicate) any())).thenReturn(resultQuery);
    when(resultQuery.distinct(true)).thenReturn(resultQuery);
    when(criteriaBuilderMock.equal(storeIdPath, STORE_ID)).thenReturn(predicate);
    when(criteriaBuilderMock.and(eq(predicate), any())).thenReturn(predicate);
    when(criteriaBuilderMock.count(any())).thenReturn(longExpression);
    when(countCriteriaQueryMock.select(longExpression)).thenReturn(countCriteriaQueryMock);
    when(productBusinessPartnerTypedQuery.setMaxResults(any(Integer.class))).thenReturn(productBusinessPartnerTypedQuery);
    when(countTypedQuery.getSingleResult()).thenReturn(TOTAL);
    when(productBusinessPartnerTypedQuery.setFirstResult(any(Integer.class)))
        .thenReturn(productBusinessPartnerTypedQuery);

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(entityManager);
  }

  @Test
  public void findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameTest() throws Exception {
    this.productBusinessPartnerCustomRepositoryBean
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(STORE_ID, BUSINESS_PARTNER_CODE, PRODUCT_NAME,
            pageable, ORDER_BY, SORT_BY);
    verify(entityManager).getCriteriaBuilder();
    verify(entityManager).createQuery(resultQuery);
    verify(entityManager).createQuery(countCriteriaQueryMock);
  }

  @Test
  public void findRejectedProductsByStoreIdAndBusinessPartnerIdTest() throws Exception {
    String sortBy = "desc";
    this.productBusinessPartnerCustomRepositoryBean
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(STORE_ID, BUSINESS_PARTNER_CODE, null,
            pageable, ORDER_BY, sortBy);
    verify(entityManager).getCriteriaBuilder();
    verify(entityManager).createQuery(resultQuery);
    verify(entityManager).createQuery(countCriteriaQueryMock);
  }
}
