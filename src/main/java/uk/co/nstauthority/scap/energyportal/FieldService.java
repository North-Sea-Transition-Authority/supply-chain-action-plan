package uk.co.nstauthority.scap.energyportal;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.generated.client.FieldProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.FieldsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.fivium.energyportalapi.generated.types.FieldStatus;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class FieldService {

  private static final List<FieldStatus> FIELD_STATUSES = Arrays.stream(FieldStatus.values())
      .filter(fieldStatus -> fieldStatus != FieldStatus.STATUS9999)
      .toList();
  static final FieldsProjectionRoot FIELDS_PROJECTION_ROOT = new FieldsProjectionRoot()
      .fieldId()
      .fieldName();
  private final FieldApi fieldApi;

  @Autowired
  FieldService(FieldApi fieldApi) {
    this.fieldApi = fieldApi;
  }

  public List<Field> getFieldsByName(String term, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    return fieldApi.searchFields(term, FIELD_STATUSES, FIELDS_PROJECTION_ROOT, requestPurpose);
  }

  public List<Field> getFieldsByIds(List<Integer> fieldIds, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    return fieldApi.getFieldsByIds(fieldIds, FIELDS_PROJECTION_ROOT, requestPurpose);
  }

  public RestSearchResult getFieldsSearchResult(List<Field> fields) {
    return new RestSearchResult(
        fields.stream()
            .map(field -> new RestSearchItem(String.valueOf(field.getFieldId()), field.getFieldName()))
            .toList());
  }

  public Optional<Field> getFieldById(Integer id, String requestPurpose) {
    var requestedFields = new FieldProjectionRoot().fieldId().fieldName();
    return fieldApi.findFieldById(id, requestedFields, new RequestPurpose(requestPurpose));
  }

  public boolean doesFieldExist(Integer id) {
    return fieldApi.findFieldById(
        id,
        new FieldProjectionRoot().fieldId(),
        new RequestPurpose("Validate that Field exists for SCAP project details"))
        .isPresent();
  }
}
