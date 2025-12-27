package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;

public class ProductItemWholesalePriceCustomRepositoryTest {

  @InjectMocks
  private ProductItemWholesalePriceCustomRepositoryImpl productItemWholesalePriceCustomRepository;

  @Mock
  private EntityManager entityManager;

  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  private CriteriaBuilder criteriaBuilderMock;
  private CriteriaQuery<ProductItemWholesalePrice> criteriaQueryMock;
  private Root<ProductItemWholesalePrice> productItemWholesalePriceRoot;
  private Predicate predicate;
  private CriteriaQuery<ProductItemWholesalePrice> resultQuery;
  private TypedQuery<ProductItemWholesalePrice> productItemWholesalePriceTypedQuery;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    criteriaBuilderMock = mock(CriteriaBuilder.class);
    resultQuery = mock(CriteriaQuery.class);
    criteriaQueryMock = mock(CriteriaQuery.class);
    productItemWholesalePriceRoot = mock(Root.class);
    predicate = mock(Predicate.class);
    Path path = mock(Path.class);
    productItemWholesalePriceTypedQuery = mock(TypedQuery.class);

    when(entityManager.getCriteriaBuilder()).thenReturn(criteriaBuilderMock);
    when(criteriaBuilderMock.createQuery(ProductItemWholesalePrice.class)).thenReturn(criteriaQueryMock);
    when(criteriaQueryMock.from(ProductItemWholesalePrice.class)).thenReturn(productItemWholesalePriceRoot);
    when(criteriaBuilderMock.equal(Mockito.any(Path.class), Mockito.anyString())).thenReturn(predicate);
    when(criteriaBuilderMock.or(Mockito.any())).thenReturn(predicate);
    when(criteriaBuilderMock.and(Mockito.any(Predicate.class), Mockito.any(Predicate.class))).thenReturn(predicate);
    when(criteriaQueryMock.select(productItemWholesalePriceRoot)).thenReturn(criteriaQueryMock);
    when(criteriaQueryMock.where(Mockito.any(Predicate.class))).thenReturn(resultQuery);
    when(entityManager.createQuery(resultQuery)).thenReturn(productItemWholesalePriceTypedQuery);
    when(productItemWholesalePriceTypedQuery.getResultList()).thenReturn(new ArrayList<>());
    when(productItemWholesalePriceRoot.get(anyString())).thenReturn(path);

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(entityManager);
  }

  @Test
  public void findProductItemWholesalePriceByItemSkuAndPickupPointCode() {
    ItemPickupPointDto itemPickupPointDto =
      ItemPickupPointDto.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build();
    productItemWholesalePriceCustomRepository.findProductItemWholesalePriceByItemSkuAndPickupPointCode(STORE_ID,
      Collections.singletonList(itemPickupPointDto));
    verify(entityManager).getCriteriaBuilder();
    verify(entityManager).createQuery(resultQuery);
  }
}
