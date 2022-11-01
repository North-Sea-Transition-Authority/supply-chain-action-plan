package uk.co.nstauthority.scap.energyportal;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.generated.client.FieldProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.FieldsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.fivium.energyportalapi.generated.types.FieldStatus;

@Service
public class FieldService {

  private final FieldApi fieldApi;

  @Autowired
  FieldService(FieldApi fieldApi) {
    this.fieldApi = fieldApi;
  }

  public List<Field> getFieldsByName(String term, String purpose) {
    var statuses = Arrays.stream(FieldStatus.values())
        .filter(fieldStatus -> fieldStatus != FieldStatus.STATUS9999)
        .toList();
    var requestedFields = new FieldsProjectionRoot()
        .fieldId()
        .fieldName();
    return fieldApi.searchFields(term, statuses, requestedFields, purpose);
  }

  public Optional<Field> getFieldById(Integer id, String requestPurpose) {
    var requestedFields = new FieldProjectionRoot().fieldId().fieldName();
    return fieldApi.findFieldById(id, requestedFields, requestPurpose);
  }

  public boolean doesFieldExist(Integer id) {
    return fieldApi.findFieldById(
        id,
        new FieldProjectionRoot().fieldId(),
        "Validate that Field exists for SCAP project details")
        .isPresent();
  }
}
