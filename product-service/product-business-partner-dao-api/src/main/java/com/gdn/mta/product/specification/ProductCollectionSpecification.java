package com.gdn.mta.product.specification;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountRequest;

public class ProductCollectionSpecification {

  private ProductCollectionSpecification() {}
  
  public static Specification<ProductCollection> countProductCollection(ProductCollectionCountRequest request) {
    return new Specification<ProductCollection>(){
      
      private List<Predicate> getQueryPredicateMap(
          Root<ProductCollection> root, CriteriaBuilder cb) {
        List<Predicate> predicateList = new ArrayList<>();
        
        switch (request.getFilterDateType()) {
          case "BETWEEN":
            predicateList.add(cb.greaterThanOrEqualTo(buildFilterByDate("submittedDate", root, cb), request.getDateStart()));
            predicateList.add(cb.lessThanOrEqualTo(buildFilterByDate("submittedDate", root, cb), request.getDateEnd()));
            break;
          case "EQUAL":
            predicateList.add(cb.equal(buildFilterByDate("submittedDate", root, cb), request.getDateStart()));
            break;
          case "LESSTHAN":
            predicateList.add(cb.lessThan(buildFilterByDate("submittedDate", root, cb), request.getDateStart()));
            break;
        }
        
        predicateList.add(cb.equal(root.get("activated"), request.isActivated()));
        predicateList.add(cb.equal(root.get("viewable"), request.isViewable()));
        predicateList.add(cb.equal(root.get("markForDelete"), false));
        
        if(StringUtils.isNotEmpty(request.getCategoryCode())){
          predicateList.add(cb.equal(root.get("categoryCode"), request.getCategoryCode()));
        }
        
        if(StringUtils.isNotEmpty(request.getBusinessPartnerCode())){
          if("INTERNAL".equals(request.getBusinessPartnerCode())){
            predicateList.add(cb.notEqual(root.get("businessPartnerCode"), request.getBusinessPartnerCode()));
          } else {
            predicateList.add(cb.equal(root.get("businessPartnerCode"), request.getBusinessPartnerCode()));
          }
        }
        
        if(StringUtils.isNotEmpty(request.getKeyword())){
          predicateList.add(cb.or(
              buildCriteriaLike(root, cb, "productCode"),
              buildCriteriaLike(root, cb, "productName"),
              buildCriteriaLike(root, cb, "createdBy")
              ));
        }
        
        if(!request.isActivated() && !request.isViewable()) {
          predicateList.add(cb.or(cb.equal(root.get("state"), "DRAFT"),cb.isNull(root.get("state"))));
        }
        
        predicateList.add(cb.equal(root.get("storeId"), request.getStoreId()));
        return predicateList;
      }
      
      private Expression<Date> buildFilterByDate(String columnKey, Root<ProductCollection> root, CriteriaBuilder cb){
        return cb.function("date", Date.class, root.get(columnKey));
      }
      
      private Predicate buildCriteriaLike(Root<ProductCollection> root, CriteriaBuilder cb, String columnKey){
        String keywordToLower = "";
        if(StringUtils.isNotBlank(request.getKeyword())) {
          keywordToLower = request.getKeyword().toLowerCase();
        }
        return cb.like(cb.lower(root.get(columnKey)), "%" + keywordToLower + "%");
      }
      
      @Override
      public Predicate toPredicate(Root<ProductCollection> root, CriteriaQuery<?> arg1,
          CriteriaBuilder cb) {
        List<Predicate> predicateList = getQueryPredicateMap(root, cb);
        return cb.and(predicateList.toArray(new Predicate[predicateList.size()]));
      }
      
    };
  }
  
}
