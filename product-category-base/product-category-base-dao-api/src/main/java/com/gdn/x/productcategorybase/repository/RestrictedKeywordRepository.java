package com.gdn.x.productcategorybase.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.RestrictedKeyword;

public interface RestrictedKeywordRepository extends JpaRepository<RestrictedKeyword, String> {

  List<RestrictedKeyword> findByStoreIdAndIdInAndMarkForDeleteFalse(String storeId, List<String> idList);

  RestrictedKeyword findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalse(String storeId, String keyword);

  List<RestrictedKeyword> findByStoreIdAndKeywordInAndMarkForDeleteFalse(String storeId, Set<String> keyword);

  Page<RestrictedKeyword> findByStoreIdAndKeywordContainingIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String keywords, Pageable pageable);

  Page<RestrictedKeyword> findByStoreIdAndMarkForDeleteFalseAndValidateOnUiNotNullAndValidateByDsNotNullOrderByUpdatedDateDesc(
      String storeId, Pageable pageable);

  Page<RestrictedKeyword> findByStoreIdAndKeywordIgnoreCaseAndMarkForDeleteFalseOrderByUpdatedDateDesc(
      String storeId, String keyword, Pageable pageable);

  List<RestrictedKeyword> findByStoreIdAndMarkForDeleteFalseAndValidateOnUi(String storeId, boolean validateOnUi);
}