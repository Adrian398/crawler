use eventscalender;
delete from event where idorganizer = 17; 
select * from event where idorganizer = 17;
select * from event;
select * from event where idorganizer in (14);
#SET foreign_key_checks = 0; ALTER TABLE event CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci; SET foreign_key_checks = 1;
#delete from event where idorganizer = 8;
#realset up ALTER DATABASE eventscalender CHARACTER SET latin1 COLLATE latin1_swedish_ci;

#ALTER DATABASE eventscalender CHARACTER SET utf8 COLLATE utf8_general_ci;

#SELECT default_character_set_name FROM information_schema.SCHEMATA S WHERE schema_name = "eventscalender";
#SELECT CCSA.character_set_name FROM information_schema.`TABLES` T,information_schema.`COLLATION_CHARACTER_SET_APPLICABILITY` CCSA WHERE CCSA.collation_name = T.table_collation AND T.table_schema = "eventscalender" AND T.table_name = "event";


SET foreign_key_checks = 0; ALTER TABLE crawler MODIFY idcrawler int(11) AUTO_INCREMENT; SET foreign_key_checks = 1;
SET foreign_key_checks = 0; TRUNCATE table organizer; SET foreign_key_checks = 1;
