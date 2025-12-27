package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.Sequence;

public interface SequenceRepository extends JpaRepository<Sequence, String> {

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query(value = "SELECT get_sequence(?1) AS sequence", nativeQuery = true)
  Long findByCode(String code);

}
