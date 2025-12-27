package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.HashSet;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.Selection;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageRequest;

import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.partners.pbp.commons.util.DatabaseFieldNames;
import com.google.common.collect.ImmutableSet;

public class ProductItemBusinessPartnerCustomRepositoryTest {

  @InjectMocks
  private ProductItemBusinessPartnerCustomRepositoryImpl productItemBusinessPartnerCustomRepository;

  @Mock
  private EntityManager entityManager;

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "productBusinessPartnerId";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final int PAGE = 1;
  private static final int SIZE = 10;
  private static final long TOTAL = 10;
  private CriteriaBuilder criteriaBuilderMock;
  private CriteriaQuery<ProductItemBusinessPartner> criteriaQueryMock;
  private Root<ProductItemBusinessPartner> productItemBusinessPartnerRoot;
  private CriteriaQuery<Long> countCriteriaQueryMock;
  private CriteriaQuery<ProductItemBusinessPartner> resultQuery;
  private TypedQuery<ProductItemBusinessPartner> productItemBusinessPartnerTypedQuery;
  private TypedQuery<Long> countTypedQuery;
  private Predicate predicate;
  private Expression<Long> longExpression;
  private CriteriaBuilder.In<Object> criteriaIn;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    resultQuery = mock(CriteriaQuery.class);
    countCriteriaQueryMock = mock(CriteriaQuery.class);
    criteriaBuilderMock = mock(CriteriaBuilder.class);
    criteriaQueryMock = mock(CriteriaQuery.class);
    productItemBusinessPartnerRoot = mock(Root.class);
    predicate = mock(Predicate.class);
    longExpression = mock(Expression.class);
    productItemBusinessPartnerTypedQuery = mock(TypedQuery.class);
    countTypedQuery = mock(TypedQuery.class);
    criteriaIn = mock(CriteriaBuilder.In.class);
    Path storeIdPath = mock(Path.class);
    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilderMock);
    when(criteriaBuilderMock.createQuery(ProductItemBusinessPartner.class)).thenReturn(criteriaQueryMock);
    when(criteriaBuilderMock.createQuery(Long.class)).thenReturn(countCriteriaQueryMock);
    when(entityManager.createQuery(resultQuery)).thenReturn(productItemBusinessPartnerTypedQuery);
    when(entityManager.createQuery(countCriteriaQueryMock)).thenReturn(countTypedQuery);
    when(criteriaQueryMock.from(ProductItemBusinessPartner.class)).thenReturn(productItemBusinessPartnerRoot);
    when(countCriteriaQueryMock.from(ProductItemBusinessPartner.class)).thenReturn(productItemBusinessPartnerRoot);
    when(productItemBusinessPartnerRoot.get(DatabaseFieldNames.STORE_ID)).thenReturn(storeIdPath);
    when(criteriaQueryMock.multiselect(any(), any(), any(), any(), any(), any(), any())).thenReturn(resultQuery);
    when(resultQuery.where((Predicate) any())).thenReturn(resultQuery);
    when(resultQuery.distinct(true)).thenReturn(resultQuery);
    when(criteriaBuilderMock.equal(storeIdPath, STORE_ID)).thenReturn(predicate);
    when(criteriaBuilderMock.and(eq(predicate), any())).thenReturn(predicate);
    when(criteriaBuilderMock.count(any())).thenReturn(longExpression);
    when(countCriteriaQueryMock.select(longExpression)).thenReturn(countCriteriaQueryMock);
    when(productItemBusinessPartnerTypedQuery.setMaxResults(any(Integer.class))).thenReturn(
        productItemBusinessPartnerTypedQuery);
    when(countTypedQuery.getSingleResult()).thenReturn(TOTAL);
    when(productItemBusinessPartnerTypedQuery.setFirstResult(any(Integer.class))).thenReturn(
        productItemBusinessPartnerTypedQuery);
    when(criteriaQueryMock.select(any())).thenReturn(criteriaQueryMock);
    when(criteriaQueryMock.where(any(Predicate.class))).thenReturn(resultQuery);
    when(criteriaBuilderMock.in(Mockito.any())).thenReturn(criteriaIn);
    when(criteriaIn.value(Mockito.any())).thenReturn(criteriaIn);
    when(criteriaBuilderMock.and(Mockito.any(), Mockito.any())).thenReturn(predicate);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(entityManager);
  }

  @Test
  public void getProductItemBusinessPartnerForNeedCorrectionTest() {
    productItemBusinessPartnerCustomRepository.findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(STORE_ID,
        PRODUCT_BUSINESS_PARTNER_ID, StringUtils.EMPTY, new HashSet<>(), PageRequest.of(PAGE, SIZE),
      false);
    verify(entityManager).getCriteriaBuilder();
    verify(entityManager).createQuery(resultQuery);
    verify(entityManager).createQuery(countCriteriaQueryMock);
  }

  @Test
  public void getProductItemBusinessPartnerForNeedCorrectionWithFiltersTest() {
    productItemBusinessPartnerCustomRepository.findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(STORE_ID,
        PRODUCT_BUSINESS_PARTNER_ID, ITEM_SKU, ImmutableSet.of(PICKUP_POINT_CODE), PageRequest.of(PAGE, SIZE),
      false);
    verify(entityManager).getCriteriaBuilder();
    verify(entityManager).createQuery(resultQuery);
    verify(entityManager).createQuery(countCriteriaQueryMock);
  }

  @Test
  public void getProductItemBusinessPartnerForNeedCorrectionFbbSortTest() {
    productItemBusinessPartnerCustomRepository.findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(STORE_ID,
      PRODUCT_BUSINESS_PARTNER_ID, StringUtils.EMPTY, new HashSet<>(), PageRequest.of(PAGE, SIZE),
      true);
    verify(entityManager).getCriteriaBuilder();
    verify(entityManager).createQuery(resultQuery);
    verify(entityManager).createQuery(countCriteriaQueryMock);
  }

}
