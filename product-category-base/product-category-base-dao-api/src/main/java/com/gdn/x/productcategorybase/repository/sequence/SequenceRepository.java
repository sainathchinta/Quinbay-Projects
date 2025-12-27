package com.gdn.x.productcategorybase.repository.sequence;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.entity.sequence.Sequence;

public interface SequenceRepository extends JpaRepository<Sequence, String> {

  String QUERY_FIND_SEQUENCE_BY_CODE = "SELECT get_sequence(?1) AS sequence";

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query(value = SequenceRepository.QUERY_FIND_SEQUENCE_BY_CODE, nativeQuery = true)
  Long findByCode(String code);

}
