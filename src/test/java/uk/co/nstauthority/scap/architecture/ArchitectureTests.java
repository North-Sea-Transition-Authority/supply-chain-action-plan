package uk.co.nstauthority.scap.architecture;

import com.tngtech.archunit.core.domain.properties.HasName;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.lang.syntax.ArchRuleDefinition;
import org.junit.jupiter.api.Test;
import uk.co.fivium.digitalenummaterialisationlibrary.enummaterialisation.MaterialisableEnum;
import uk.co.nstauthority.scap.ScapApplication;
import uk.co.nstauthority.scap.enumutil.Displayable;

class ArchitectureTests {

  @Test
  void displayableEnumsAreMaterialised() {
    var javaClasses = new ClassFileImporter()
        .withImportOption(ImportOption.Predefined.DO_NOT_INCLUDE_TESTS)
        .importPackagesOf(ScapApplication.class);

    ArchRuleDefinition.classes()
        .that().areEnums()
        .and().containAnyMethodsThat(
            HasName.Predicates.name("getDisplayName").or(HasName.Predicates.name("getDisplayOrder"))
        )
        .should().implement(MaterialisableEnum.class)
        .check(javaClasses);

    ArchRuleDefinition.theClass(Displayable.class)
        .should().beAssignableTo(MaterialisableEnum.class);
  }
}
